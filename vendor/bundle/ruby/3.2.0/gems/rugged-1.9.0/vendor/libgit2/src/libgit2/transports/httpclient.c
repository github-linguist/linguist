/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "git2.h"

#include "vector.h"
#include "trace.h"
#include "httpclient.h"
#include "http.h"
#include "auth.h"
#include "auth_negotiate.h"
#include "auth_ntlm.h"
#include "git2/sys/credential.h"
#include "net.h"
#include "stream.h"
#include "streams/socket.h"
#include "streams/tls.h"
#include "auth.h"
#include "httpparser.h"

static git_http_auth_scheme auth_schemes[] = {
	{ GIT_HTTP_AUTH_NEGOTIATE, "Negotiate", GIT_CREDENTIAL_DEFAULT, git_http_auth_negotiate },
	{ GIT_HTTP_AUTH_NTLM, "NTLM", GIT_CREDENTIAL_USERPASS_PLAINTEXT, git_http_auth_ntlm },
	{ GIT_HTTP_AUTH_BASIC, "Basic", GIT_CREDENTIAL_USERPASS_PLAINTEXT, git_http_auth_basic },
};

/*
 * Use a 16kb read buffer to match the maximum size of a TLS packet.  This
 * is critical for compatibility with SecureTransport, which will always do
 * a network read on every call, even if it has data buffered to return to
 * you.  That buffered data may be the _end_ of a keep-alive response, so
 * if SecureTransport performs another network read, it will wait until the
 * server ultimately times out before it returns that buffered data to you.
 * Since SecureTransport only reads a single TLS packet at a time, by
 * calling it with a read buffer that is the maximum size of a TLS packet,
 * we ensure that it will never buffer.
 */
#define GIT_READ_BUFFER_SIZE (16 * 1024)

typedef struct {
	git_net_url url;
	git_stream *stream;

	git_vector auth_challenges;
	git_http_auth_context *auth_context;
} git_http_server;

typedef enum {
	PROXY = 1,
	SERVER
} git_http_server_t;

typedef enum {
	NONE = 0,
	SENDING_REQUEST,
	SENDING_BODY,
	SENT_REQUEST,
	HAS_EARLY_RESPONSE,
	READING_RESPONSE,
	READING_BODY,
	DONE
} http_client_state;

/* Parser state */
typedef enum {
	PARSE_HEADER_NONE = 0,
	PARSE_HEADER_NAME,
	PARSE_HEADER_VALUE,
	PARSE_HEADER_COMPLETE
} parse_header_state;

typedef enum {
	PARSE_STATUS_OK,
	PARSE_STATUS_NO_OUTPUT,
	PARSE_STATUS_ERROR
} parse_status;

typedef struct {
	git_http_client *client;
	git_http_response *response;

	/* Temporary buffers to avoid extra mallocs */
	git_str parse_header_name;
	git_str parse_header_value;

	/* Parser state */
	int error;
	parse_status parse_status;

	/* Headers parsing */
	parse_header_state parse_header_state;

	/* Body parsing */
	char *output_buf;       /* Caller's output buffer */
	size_t output_size;     /* Size of caller's output buffer */
	size_t output_written;  /* Bytes we've written to output buffer */
} http_parser_context;

/* HTTP client connection */
struct git_http_client {
	git_http_client_options opts;

	/* Are we writing to the proxy or server, and state of the client. */
	git_http_server_t current_server;
	http_client_state state;

	git_http_parser parser;

	git_http_server server;
	git_http_server proxy;

	unsigned request_count;
	unsigned connected : 1,
	         proxy_connected : 1,
	         keepalive : 1,
	         request_chunked : 1;

	/* Temporary buffers to avoid extra mallocs */
	git_str request_msg;
	git_str read_buf;

	/* A subset of information from the request */
	size_t request_body_len,
	       request_body_remain;

	/*
	 * When state == HAS_EARLY_RESPONSE, the response of our proxy
	 * that we have buffered and will deliver during read_response.
	 */
	git_http_response early_response;
};

bool git_http_response_is_redirect(git_http_response *response)
{
	return (response->status == GIT_HTTP_MOVED_PERMANENTLY ||
	        response->status == GIT_HTTP_FOUND ||
	        response->status == GIT_HTTP_SEE_OTHER ||
		response->status == GIT_HTTP_TEMPORARY_REDIRECT ||
		response->status == GIT_HTTP_PERMANENT_REDIRECT);
}

void git_http_response_dispose(git_http_response *response)
{
	if (!response)
		return;

	git__free(response->content_type);
	git__free(response->location);

	memset(response, 0, sizeof(git_http_response));
}

static int on_header_complete(git_http_parser *parser)
{
	http_parser_context *ctx = (http_parser_context *) parser->data;
	git_http_client *client = ctx->client;
	git_http_response *response = ctx->response;

	git_str *name = &ctx->parse_header_name;
	git_str *value = &ctx->parse_header_value;

	if (!strcasecmp("Content-Type", name->ptr)) {
		if (response->content_type) {
			git_error_set(GIT_ERROR_HTTP,
			              "multiple content-type headers");
			return -1;
		}

		response->content_type =
			git__strndup(value->ptr, value->size);
		GIT_ERROR_CHECK_ALLOC(ctx->response->content_type);
	} else if (!strcasecmp("Content-Length", name->ptr)) {
		int64_t len;

		if (response->content_length) {
			git_error_set(GIT_ERROR_HTTP,
			              "multiple content-length headers");
			return -1;
		}

		if (git__strntol64(&len, value->ptr, value->size,
		                   NULL, 10) < 0 || len < 0) {
			git_error_set(GIT_ERROR_HTTP,
			              "invalid content-length");
			return -1;
		}

		response->content_length = (size_t)len;
	} else if (!strcasecmp("Transfer-Encoding", name->ptr) &&
	           !strcasecmp("chunked", value->ptr)) {
			ctx->response->chunked = 1;
	} else if (!strcasecmp("Proxy-Authenticate", git_str_cstr(name))) {
		char *dup = git__strndup(value->ptr, value->size);
		GIT_ERROR_CHECK_ALLOC(dup);

		if (git_vector_insert(&client->proxy.auth_challenges, dup) < 0)
			return -1;
	} else if (!strcasecmp("WWW-Authenticate", name->ptr)) {
		char *dup = git__strndup(value->ptr, value->size);
		GIT_ERROR_CHECK_ALLOC(dup);

		if (git_vector_insert(&client->server.auth_challenges, dup) < 0)
			return -1;
	} else if (!strcasecmp("Location", name->ptr)) {
		if (response->location) {
			git_error_set(GIT_ERROR_HTTP,
				"multiple location headers");
			return -1;
		}

		response->location = git__strndup(value->ptr, value->size);
		GIT_ERROR_CHECK_ALLOC(response->location);
	}

	return 0;
}

static int on_header_field(git_http_parser *parser, const char *str, size_t len)
{
	http_parser_context *ctx = (http_parser_context *) parser->data;

	switch (ctx->parse_header_state) {
	/*
	 * We last saw a header value, process the name/value pair and
	 * get ready to handle this new name.
	 */
	case PARSE_HEADER_VALUE:
		if (on_header_complete(parser) < 0)
			return ctx->parse_status = PARSE_STATUS_ERROR;

		git_str_clear(&ctx->parse_header_name);
		git_str_clear(&ctx->parse_header_value);
		/* Fall through */

	case PARSE_HEADER_NONE:
	case PARSE_HEADER_NAME:
		ctx->parse_header_state = PARSE_HEADER_NAME;

		if (git_str_put(&ctx->parse_header_name, str, len) < 0)
			return ctx->parse_status = PARSE_STATUS_ERROR;

		break;

	default:
		git_error_set(GIT_ERROR_HTTP,
		              "header name seen at unexpected time");
		return ctx->parse_status = PARSE_STATUS_ERROR;
	}

	return 0;
}

static int on_header_value(git_http_parser *parser, const char *str, size_t len)
{
	http_parser_context *ctx = (http_parser_context *) parser->data;

	switch (ctx->parse_header_state) {
	case PARSE_HEADER_NAME:
	case PARSE_HEADER_VALUE:
		ctx->parse_header_state = PARSE_HEADER_VALUE;

		if (git_str_put(&ctx->parse_header_value, str, len) < 0)
			return ctx->parse_status = PARSE_STATUS_ERROR;

		break;

	default:
		git_error_set(GIT_ERROR_HTTP,
		              "header value seen at unexpected time");
		return ctx->parse_status = PARSE_STATUS_ERROR;
	}

	return 0;
}

GIT_INLINE(bool) challenge_matches_scheme(
	const char *challenge,
	git_http_auth_scheme *scheme)
{
	const char *scheme_name = scheme->name;
	size_t scheme_len = strlen(scheme_name);

	if (!strncasecmp(challenge, scheme_name, scheme_len) &&
	    (challenge[scheme_len] == '\0' || challenge[scheme_len] == ' '))
		return true;

	return false;
}

static git_http_auth_scheme *scheme_for_challenge(const char *challenge)
{
	size_t i;

	for (i = 0; i < ARRAY_SIZE(auth_schemes); i++) {
		if (challenge_matches_scheme(challenge, &auth_schemes[i]))
			return &auth_schemes[i];
	}

	return NULL;
}

GIT_INLINE(void) collect_authinfo(
	unsigned int *schemetypes,
	unsigned int *credtypes,
	git_vector *challenges)
{
	git_http_auth_scheme *scheme;
	const char *challenge;
	size_t i;

	*schemetypes = 0;
	*credtypes = 0;

	git_vector_foreach(challenges, i, challenge) {
		if ((scheme = scheme_for_challenge(challenge)) != NULL) {
			*schemetypes |= scheme->type;
			*credtypes |= scheme->credtypes;
		}
	}
}

static int resend_needed(git_http_client *client, git_http_response *response)
{
	git_http_auth_context *auth_context;

	if (response->status == GIT_HTTP_STATUS_UNAUTHORIZED &&
	    (auth_context = client->server.auth_context) &&
	    auth_context->is_complete &&
	    !auth_context->is_complete(auth_context))
		return 1;

	if (response->status == GIT_HTTP_STATUS_PROXY_AUTHENTICATION_REQUIRED &&
	    (auth_context = client->proxy.auth_context) &&
	    auth_context->is_complete &&
	    !auth_context->is_complete(auth_context))
		return 1;

	return 0;
}

static int on_headers_complete(git_http_parser *parser)
{
	http_parser_context *ctx = (http_parser_context *) parser->data;

	/* Finalize the last seen header */
	switch (ctx->parse_header_state) {
	case PARSE_HEADER_VALUE:
		if (on_header_complete(parser) < 0)
			return ctx->parse_status = PARSE_STATUS_ERROR;

		/* Fall through */

	case PARSE_HEADER_NONE:
		ctx->parse_header_state = PARSE_HEADER_COMPLETE;
		break;

	default:
		git_error_set(GIT_ERROR_HTTP,
		              "header completion at unexpected time");
		return ctx->parse_status = PARSE_STATUS_ERROR;
	}

	ctx->response->status = git_http_parser_status_code(parser);
	ctx->client->keepalive = git_http_parser_keep_alive(parser);

	/* Prepare for authentication */
	collect_authinfo(&ctx->response->server_auth_schemetypes,
	                 &ctx->response->server_auth_credtypes,
	                 &ctx->client->server.auth_challenges);
	collect_authinfo(&ctx->response->proxy_auth_schemetypes,
	                 &ctx->response->proxy_auth_credtypes,
	                 &ctx->client->proxy.auth_challenges);

	ctx->response->resend_credentials = resend_needed(ctx->client,
	                                                  ctx->response);

	if (ctx->response->content_type || ctx->response->chunked)
		ctx->client->state = READING_BODY;
	else
		ctx->client->state = DONE;

	return git_http_parser_pause(parser);
}

static int on_body(git_http_parser *parser, const char *buf, size_t len)
{
	http_parser_context *ctx = (http_parser_context *) parser->data;
	size_t max_len;

	/* Saw data when we expected not to (eg, in consume_response_body) */
	if (ctx->output_buf == NULL || ctx->output_size == 0) {
		ctx->parse_status = PARSE_STATUS_NO_OUTPUT;
		return 0;
	}

	GIT_ASSERT(ctx->output_size >= ctx->output_written);

	max_len = min(ctx->output_size - ctx->output_written, len);
	max_len = min(max_len, INT_MAX);

	memcpy(ctx->output_buf + ctx->output_written, buf, max_len);
	ctx->output_written += max_len;

	return 0;
}

static int on_message_complete(git_http_parser *parser)
{
	http_parser_context *ctx = (http_parser_context *) parser->data;

	ctx->client->state = DONE;
	return 0;
}

GIT_INLINE(int) stream_write(
	git_http_server *server,
	const char *data,
	size_t len)
{
	git_trace(GIT_TRACE_TRACE,
	          "Sending request:\n%.*s", (int)len, data);

	return git_stream__write_full(server->stream, data, len, 0);
}

GIT_INLINE(int) client_write_request(git_http_client *client)
{
	git_stream *stream = client->current_server == PROXY ?
		             client->proxy.stream : client->server.stream;

	git_trace(GIT_TRACE_TRACE,
	          "Sending request:\n%.*s",
	          (int)client->request_msg.size, client->request_msg.ptr);

	return git_stream__write_full(stream,
				      client->request_msg.ptr,
	                              client->request_msg.size,
				      0);
}

static const char *name_for_method(git_http_method method)
{
	switch (method) {
	case GIT_HTTP_METHOD_GET:
		return "GET";
	case GIT_HTTP_METHOD_POST:
		return "POST";
	case GIT_HTTP_METHOD_CONNECT:
		return "CONNECT";
	}

	return NULL;
}

/*
 * Find the scheme that is suitable for the given credentials, based on the
 * server's auth challenges.
 */
static bool best_scheme_and_challenge(
	git_http_auth_scheme **scheme_out,
	const char **challenge_out,
	git_vector *challenges,
	git_credential *credentials)
{
	const char *challenge;
	size_t i, j;

	for (i = 0; i < ARRAY_SIZE(auth_schemes); i++) {
		git_vector_foreach(challenges, j, challenge) {
			git_http_auth_scheme *scheme = &auth_schemes[i];

			if (challenge_matches_scheme(challenge, scheme) &&
			    (scheme->credtypes & credentials->credtype)) {
				*scheme_out = scheme;
				*challenge_out = challenge;
				return true;
			}
		}
	}

	return false;
}

/*
 * Find the challenge from the server for our current auth context.
 */
static const char *challenge_for_context(
	git_vector *challenges,
	git_http_auth_context *auth_ctx)
{
	const char *challenge;
	size_t i, j;

	for (i = 0; i < ARRAY_SIZE(auth_schemes); i++) {
		if (auth_schemes[i].type == auth_ctx->type) {
			git_http_auth_scheme *scheme = &auth_schemes[i];

			git_vector_foreach(challenges, j, challenge) {
				if (challenge_matches_scheme(challenge, scheme))
					return challenge;
			}
		}
	}

	return NULL;
}

static const char *init_auth_context(
	git_http_server *server,
	git_vector *challenges,
	git_credential *credentials)
{
	git_http_auth_scheme *scheme;
	const char *challenge;
	int error;

	if (!best_scheme_and_challenge(&scheme, &challenge, challenges, credentials)) {
		git_error_set(GIT_ERROR_HTTP, "could not find appropriate mechanism for credentials");
		return NULL;
	}

	error = scheme->init_context(&server->auth_context, &server->url);

	if (error == GIT_PASSTHROUGH) {
		git_error_set(GIT_ERROR_HTTP, "'%s' authentication is not supported", scheme->name);
		return NULL;
	}

	return challenge;
}

static void free_auth_context(git_http_server *server)
{
	if (!server->auth_context)
		return;

	if (server->auth_context->free)
		server->auth_context->free(server->auth_context);

	server->auth_context = NULL;
}

static int apply_credentials(
	git_str *buf,
	git_http_server *server,
	const char *header_name,
	git_credential *credentials)
{
	git_http_auth_context *auth = server->auth_context;
	git_vector *challenges = &server->auth_challenges;
	const char *challenge;
	git_str token = GIT_STR_INIT;
	int error = 0;

	/* We've started a new request without creds; free the context. */
	if (auth && !credentials) {
		free_auth_context(server);
		return 0;
	}

	/* We haven't authenticated, nor were we asked to.  Nothing to do. */
	if (!auth && !git_vector_length(challenges))
		return 0;

	if (!auth) {
		challenge = init_auth_context(server, challenges, credentials);
		auth = server->auth_context;

		if (!challenge || !auth) {
			error = -1;
			goto done;
		}
	} else if (auth->set_challenge) {
		challenge = challenge_for_context(challenges, auth);
	}

	if (auth->set_challenge && challenge &&
	    (error = auth->set_challenge(auth, challenge)) < 0)
		goto done;

	if ((error = auth->next_token(&token, auth, credentials)) < 0)
		goto done;

	if (auth->is_complete && auth->is_complete(auth)) {
		/*
		 * If we're done with an auth mechanism with connection affinity,
		 * we don't need to send any more headers and can dispose the context.
		 */
		if (auth->connection_affinity)
			free_auth_context(server);
	} else if (!token.size) {
		git_error_set(GIT_ERROR_HTTP, "failed to respond to authentication challenge");
		error = GIT_EAUTH;
		goto done;
	}

	if (token.size > 0)
		error = git_str_printf(buf, "%s: %s\r\n", header_name, token.ptr);

done:
	git_str_dispose(&token);
	return error;
}

GIT_INLINE(int) apply_server_credentials(
	git_str *buf,
	git_http_client *client,
	git_http_request *request)
{
	return apply_credentials(buf,
	                         &client->server,
	                         "Authorization",
	                         request->credentials);
}

GIT_INLINE(int) apply_proxy_credentials(
	git_str *buf,
	git_http_client *client,
	git_http_request *request)
{
	return apply_credentials(buf,
	                         &client->proxy,
	                         "Proxy-Authorization",
	                         request->proxy_credentials);
}

static int puts_host_and_port(git_str *buf, git_net_url *url, bool force_port)
{
	bool ipv6 = git_net_url_is_ipv6(url);

	if (ipv6)
		git_str_putc(buf, '[');

	git_str_puts(buf, url->host);

	if (ipv6)
		git_str_putc(buf, ']');

	if (force_port || !git_net_url_is_default_port(url)) {
		git_str_putc(buf, ':');
		git_str_puts(buf, url->port);
	}

	return git_str_oom(buf) ? -1 : 0;
}

static int append_user_agent(git_str *buf)
{
	const char *product = git_settings__user_agent_product();
	const char *comment = git_settings__user_agent();

	GIT_ASSERT(product && comment);

	if (!*product)
		return 0;

	git_str_puts(buf, "User-Agent: ");
	git_str_puts(buf, product);

	if (*comment) {
		git_str_puts(buf, " (");
		git_str_puts(buf, comment);
		git_str_puts(buf, ")");
	}

	git_str_puts(buf, "\r\n");

	return git_str_oom(buf) ? -1 : 0;
}

static int generate_connect_request(
	git_http_client *client,
	git_http_request *request)
{
	git_str *buf;
	int error;

	git_str_clear(&client->request_msg);
	buf = &client->request_msg;

	git_str_puts(buf, "CONNECT ");
	puts_host_and_port(buf, &client->server.url, true);
	git_str_puts(buf, " HTTP/1.1\r\n");

	append_user_agent(buf);

	git_str_puts(buf, "Host: ");
	puts_host_and_port(buf, &client->server.url, true);
	git_str_puts(buf, "\r\n");

	if ((error = apply_proxy_credentials(buf, client, request) < 0))
		return -1;

	git_str_puts(buf, "\r\n");

	return git_str_oom(buf) ? -1 : 0;
}

static bool use_connect_proxy(git_http_client *client)
{
    return client->proxy.url.host && !strcmp(client->server.url.scheme, "https");
}

static int generate_request(
	git_http_client *client,
	git_http_request *request)
{
	git_str *buf;
	size_t i;
	int error;

	GIT_ASSERT_ARG(client);
	GIT_ASSERT_ARG(request);

	git_str_clear(&client->request_msg);
	buf = &client->request_msg;

	/* GET|POST path HTTP/1.1 */
	git_str_puts(buf, name_for_method(request->method));
	git_str_putc(buf, ' ');

	if (request->proxy && strcmp(request->url->scheme, "https"))
		git_net_url_fmt(buf, request->url);
	else
		git_net_url_fmt_path(buf, request->url);

	git_str_puts(buf, " HTTP/1.1\r\n");

	append_user_agent(buf);

	git_str_puts(buf, "Host: ");
	puts_host_and_port(buf, request->url, false);
	git_str_puts(buf, "\r\n");

	if (request->accept)
		git_str_printf(buf, "Accept: %s\r\n", request->accept);
	else
		git_str_puts(buf, "Accept: */*\r\n");

	if (request->content_type)
		git_str_printf(buf, "Content-Type: %s\r\n",
			request->content_type);

	if (request->chunked)
		git_str_puts(buf, "Transfer-Encoding: chunked\r\n");

	if (request->content_length > 0)
		git_str_printf(buf, "Content-Length: %"PRIuZ "\r\n",
			request->content_length);

	if (request->expect_continue)
		git_str_printf(buf, "Expect: 100-continue\r\n");

	if ((error = apply_server_credentials(buf, client, request)) < 0 ||
	    (!use_connect_proxy(client) &&
			(error = apply_proxy_credentials(buf, client, request)) < 0))
		return error;

	if (request->custom_headers) {
		for (i = 0; i < request->custom_headers->count; i++) {
			const char *hdr = request->custom_headers->strings[i];

			if (hdr)
				git_str_printf(buf, "%s\r\n", hdr);
		}
	}

	git_str_puts(buf, "\r\n");

	if (git_str_oom(buf))
		return -1;

	return 0;
}

static int check_certificate(
	git_stream *stream,
	git_net_url *url,
	int is_valid,
	git_transport_certificate_check_cb cert_cb,
	void *cert_cb_payload)
{
	git_cert *cert;
	git_error *last_error;
	int error;

	if ((error = git_stream_certificate(&cert, stream)) < 0)
		return error;

	/*
	 * Allow callers to set an error - but save ours and clear
	 * it, so that we can detect if they set one and restore it
	 * if we need to.
	 */
	git_error_save(&last_error);
	git_error_clear();

	error = cert_cb(cert, is_valid, url->host, cert_cb_payload);

	if (error == GIT_PASSTHROUGH) {
		error = is_valid ? 0 : -1;

		if (error) {
			git_error_restore(last_error);
			last_error = NULL;
		}
	} else if (error) {
		if (!git_error_exists())
			git_error_set(GIT_ERROR_HTTP,
		              "user rejected certificate for %s",
			      url->host);
	}

	git_error_free(last_error);
	return error;
}

static int server_connect_stream(
	git_http_server *server,
	git_transport_certificate_check_cb cert_cb,
	void *cb_payload)
{
	int error;

	GIT_ERROR_CHECK_VERSION(server->stream, GIT_STREAM_VERSION, "git_stream");

	error = git_stream_connect(server->stream);

	if (error && error != GIT_ECERTIFICATE)
		return error;

	if (git_stream_is_encrypted(server->stream) && cert_cb != NULL)
		error = check_certificate(server->stream, &server->url, !error,
		                          cert_cb, cb_payload);

	return error;
}

static void reset_auth_connection(git_http_server *server)
{
	/*
	 * If we've authenticated and we're doing "normal"
	 * authentication with a request affinity (Basic, Digest)
	 * then we want to _keep_ our context, since authentication
	 * survives even through non-keep-alive connections.  If
	 * we've authenticated and we're doing connection-based
	 * authentication (NTLM, Negotiate) - indicated by the presence
	 * of an `is_complete` callback - then we need to restart
	 * authentication on a new connection.
	 */

	if (server->auth_context &&
	    server->auth_context->connection_affinity)
		free_auth_context(server);
}

/*
 * Updates the server data structure with the new URL; returns 1 if the server
 * has changed and we need to reconnect, returns 0 otherwise.
 */
GIT_INLINE(int) server_setup_from_url(
	git_http_server *server,
	git_net_url *url)
{
	GIT_ASSERT_ARG(url);
	GIT_ASSERT_ARG(url->scheme);
	GIT_ASSERT_ARG(url->host);
	GIT_ASSERT_ARG(url->port);

	if (!server->url.scheme || strcmp(server->url.scheme, url->scheme) ||
	    !server->url.host || strcmp(server->url.host, url->host) ||
	    !server->url.port || strcmp(server->url.port, url->port)) {
		git__free(server->url.scheme);
		git__free(server->url.host);
		git__free(server->url.port);

		server->url.scheme = git__strdup(url->scheme);
		GIT_ERROR_CHECK_ALLOC(server->url.scheme);

		server->url.host = git__strdup(url->host);
		GIT_ERROR_CHECK_ALLOC(server->url.host);

		server->url.port = git__strdup(url->port);
		GIT_ERROR_CHECK_ALLOC(server->url.port);

		return 1;
	}

	return 0;
}

static bool parser_settings_initialized;
static git_http_parser_settings parser_settings;

GIT_INLINE(git_http_parser_settings *) http_client_parser_settings(void)
{
	if (!parser_settings_initialized) {
		parser_settings.on_header_field = on_header_field;
		parser_settings.on_header_value = on_header_value;
		parser_settings.on_headers_complete = on_headers_complete;
		parser_settings.on_body = on_body;
		parser_settings.on_message_complete = on_message_complete;

		parser_settings_initialized = true;
	}

	return &parser_settings;
}

static void reset_parser(git_http_client *client)
{
	git_http_parser_init(&client->parser,
	                     GIT_HTTP_PARSER_RESPONSE,
	                     http_client_parser_settings());
}

static int setup_hosts(
	git_http_client *client,
	git_http_request *request)
{
	int ret, diff = 0;

	GIT_ASSERT_ARG(client);
	GIT_ASSERT_ARG(request);

	GIT_ASSERT(request->url);

	if ((ret = server_setup_from_url(&client->server, request->url)) < 0)
		return ret;

	diff |= ret;

	if (request->proxy &&
	    (ret = server_setup_from_url(&client->proxy, request->proxy)) < 0)
		return ret;

	diff |= ret;

	if (diff) {
		free_auth_context(&client->server);
		free_auth_context(&client->proxy);

		client->connected = 0;
	}

	return 0;
}

GIT_INLINE(int) server_create_stream(git_http_server *server)
{
	git_net_url *url = &server->url;

	if (strcasecmp(url->scheme, "https") == 0)
		return git_tls_stream_new(&server->stream, url->host, url->port);
	else if (strcasecmp(url->scheme, "http") == 0)
		return git_socket_stream_new(&server->stream, url->host, url->port);

	git_error_set(GIT_ERROR_HTTP, "unknown http scheme '%s'", url->scheme);
	return -1;
}

GIT_INLINE(void) save_early_response(
	git_http_client *client,
	git_http_response *response)
{
	/* Buffer the response so we can return it in read_response */
	client->state = HAS_EARLY_RESPONSE;

	memcpy(&client->early_response, response, sizeof(git_http_response));
	memset(response, 0, sizeof(git_http_response));
}

static int proxy_connect(
	git_http_client *client,
	git_http_request *request)
{
	git_http_response response = {0};
	int error;

	if (!client->proxy_connected || !client->keepalive) {
		git_trace(GIT_TRACE_DEBUG, "Connecting to proxy %s port %s",
			  client->proxy.url.host, client->proxy.url.port);

		if ((error = server_create_stream(&client->proxy)) < 0 ||
		    (error = server_connect_stream(&client->proxy,
			client->opts.proxy_certificate_check_cb,
			client->opts.proxy_certificate_check_payload)) < 0)
			goto done;

		client->proxy_connected = 1;
	}

	client->current_server = PROXY;
	client->state = SENDING_REQUEST;

	if ((error = generate_connect_request(client, request)) < 0 ||
	    (error = client_write_request(client)) < 0)
		goto done;

	client->state = SENT_REQUEST;

	if ((error = git_http_client_read_response(&response, client)) < 0 ||
	    (error = git_http_client_skip_body(client)) < 0)
		goto done;

	GIT_ASSERT(client->state == DONE);

	if (response.status == GIT_HTTP_STATUS_PROXY_AUTHENTICATION_REQUIRED) {
		save_early_response(client, &response);

		error = GIT_RETRY;
		goto done;
	} else if (response.status != GIT_HTTP_STATUS_OK) {
		git_error_set(GIT_ERROR_HTTP, "proxy returned unexpected status: %d", response.status);
		error = -1;
		goto done;
	}

	reset_parser(client);
	client->state = NONE;

done:
	git_http_response_dispose(&response);
	return error;
}

static int server_connect(git_http_client *client)
{
	git_net_url *url = &client->server.url;
	git_transport_certificate_check_cb cert_cb;
	void *cert_payload;
	int error;

	client->current_server = SERVER;

	if (client->proxy.stream)
		error = git_tls_stream_wrap(&client->server.stream, client->proxy.stream, url->host);
	else
		error = server_create_stream(&client->server);

	if (error < 0)
		goto done;

	cert_cb = client->opts.server_certificate_check_cb;
	cert_payload = client->opts.server_certificate_check_payload;

	error = server_connect_stream(&client->server, cert_cb, cert_payload);

done:
	return error;
}

GIT_INLINE(void) close_stream(git_http_server *server)
{
	if (server->stream) {
		git_stream_close(server->stream);
		git_stream_free(server->stream);
		server->stream = NULL;
	}
}

static int http_client_connect(
	git_http_client *client,
	git_http_request *request)
{
	bool use_proxy = false;
	int error;

	if ((error = setup_hosts(client, request)) < 0)
		goto on_error;

	/* We're connected to our destination server; no need to reconnect */
	if (client->connected && client->keepalive &&
	    (client->state == NONE || client->state == DONE))
		return 0;

	client->connected = 0;
	client->request_count = 0;

	close_stream(&client->server);
	reset_auth_connection(&client->server);

	reset_parser(client);

	/* Reconnect to the proxy if necessary. */
	use_proxy = use_connect_proxy(client);

	if (use_proxy) {
		if (!client->proxy_connected || !client->keepalive ||
		    (client->state != NONE && client->state != DONE)) {
			close_stream(&client->proxy);
			reset_auth_connection(&client->proxy);

			client->proxy_connected = 0;
		}

		if ((error = proxy_connect(client, request)) < 0)
			goto on_error;
	}

	git_trace(GIT_TRACE_DEBUG, "Connecting to remote %s port %s",
	          client->server.url.host, client->server.url.port);

	if ((error = server_connect(client)) < 0)
		goto on_error;

	client->connected = 1;
	return error;

on_error:
	if (error != GIT_RETRY)
		close_stream(&client->proxy);

	close_stream(&client->server);
	return error;
}

GIT_INLINE(int) client_read(git_http_client *client)
{
	http_parser_context *parser_context = client->parser.data;
	git_stream *stream;
	char *buf = client->read_buf.ptr + client->read_buf.size;
	size_t max_len;
	ssize_t read_len;

	stream = client->current_server == PROXY ?
		client->proxy.stream : client->server.stream;

	/*
	 * We use a git_str for convenience, but statically allocate it and
	 * don't resize.  Limit our consumption to INT_MAX since calling
	 * functions use an int return type to return number of bytes read.
	 */
	max_len = client->read_buf.asize - client->read_buf.size;
	max_len = min(max_len, INT_MAX);

	if (parser_context->output_size)
		max_len = min(max_len, parser_context->output_size);

	if (max_len == 0) {
		git_error_set(GIT_ERROR_HTTP, "no room in output buffer");
		return -1;
	}

	read_len = git_stream_read(stream, buf, max_len);

	if (read_len >= 0) {
		client->read_buf.size += read_len;

		git_trace(GIT_TRACE_TRACE, "Received:\n%.*s",
		          (int)read_len, buf);
	}

	return (int)read_len;
}

GIT_INLINE(int) client_read_and_parse(git_http_client *client)
{
	git_http_parser *parser = &client->parser;
	http_parser_context *ctx = (http_parser_context *) parser->data;
	unsigned char http_errno;
	int read_len;
	size_t parsed_len;

	/*
	 * If we have data in our read buffer, that means we stopped early
	 * when parsing headers.  Use the data in the read buffer instead of
	 * reading more from the socket.
	 */
	if (!client->read_buf.size && (read_len = client_read(client)) < 0)
		return read_len;

	parsed_len = git_http_parser_execute(parser,
		client->read_buf.ptr,
		client->read_buf.size);
	http_errno = git_http_parser_errno(parser);

	if (parsed_len > INT_MAX) {
		git_error_set(GIT_ERROR_HTTP, "unexpectedly large parse");
		return -1;
	}

	if (ctx->parse_status == PARSE_STATUS_ERROR) {
		client->connected = 0;
		return ctx->error ? ctx->error : -1;
	}

	/*
	 * If we finished reading the headers or body, we paused parsing.
	 * Otherwise the parser will start filling the body, or even parse
	 * a new response if the server pipelined us multiple responses.
	 * (This can happen in response to an expect/continue request,
	 * where the server gives you a 100 and 200 simultaneously.)
	 */
	if (http_errno == GIT_HTTP_PARSER_PAUSED) {
		size_t additional_size;

		git_http_parser_resume(parser);

		/*
		 * http-parser has a "feature" where it will not deliver
		 * the final byte when paused in a callback.  Consume
		 * that byte.
		 */
		if ((additional_size = git_http_parser_remain_after_pause(parser)) > 0) {
			GIT_ASSERT((client->read_buf.size - parsed_len) >= additional_size);

			parsed_len += git_http_parser_execute(parser,
				client->read_buf.ptr + parsed_len,
				additional_size);
		}
	}

	/* Most failures will be reported in http_errno */
	else if (git_http_parser_errno(parser) != GIT_HTTP_PARSER_OK) {
		git_error_set(GIT_ERROR_HTTP, "http parser error: %s",
		              git_http_parser_errmsg(parser, http_errno));
		return -1;
	}

	/* Otherwise we should have consumed the entire buffer. */
	else if (parsed_len != client->read_buf.size) {
		git_error_set(GIT_ERROR_HTTP,
		              "http parser did not consume entire buffer: %s",
		              git_http_parser_errmsg(parser, http_errno));
		return -1;
	}

	/* recv returned 0, the server hung up on us */
	else if (!parsed_len) {
		git_error_set(GIT_ERROR_HTTP, "unexpected EOF");
		return -1;
	}

	git_str_consume_bytes(&client->read_buf, parsed_len);

	return (int)parsed_len;
}

/*
 * See if we've consumed the entire response body.  If the client was
 * reading the body but did not consume it entirely, it's possible that
 * they knew that the stream had finished (in a git response, seeing a
 * final flush) and stopped reading.  But if the response was chunked,
 * we may have not consumed the final chunk marker.  Consume it to
 * ensure that we don't have it waiting in our socket.  If there's
 * more than just a chunk marker, close the connection.
 */
static void complete_response_body(git_http_client *client)
{
	http_parser_context parser_context = {0};

	/* If we're not keeping alive, don't bother. */
	if (!client->keepalive) {
		client->connected = 0;
		goto done;
	}

	parser_context.client = client;
	client->parser.data = &parser_context;

	/* If there was an error, just close the connection. */
	if (client_read_and_parse(client) < 0 ||
	    parser_context.error != GIT_HTTP_PARSER_OK ||
	    (parser_context.parse_status != PARSE_STATUS_OK &&
	     parser_context.parse_status != PARSE_STATUS_NO_OUTPUT)) {
		git_error_clear();
		client->connected = 0;
	}

done:
	client->parser.data = NULL;
	git_str_clear(&client->read_buf);
}

int git_http_client_send_request(
	git_http_client *client,
	git_http_request *request)
{
	git_http_response response = {0};
	int error = -1;

	GIT_ASSERT_ARG(client);
	GIT_ASSERT_ARG(request);

	/* If the client did not finish reading, clean up the stream. */
	if (client->state == READING_BODY)
		complete_response_body(client);

	/* If we're waiting for proxy auth, don't sending more requests. */
	if (client->state == HAS_EARLY_RESPONSE)
		return 0;

	if (git_trace_level() >= GIT_TRACE_DEBUG) {
		git_str url = GIT_STR_INIT;
		git_net_url_fmt(&url, request->url);
		git_trace(GIT_TRACE_DEBUG, "Sending %s request to %s",
		          name_for_method(request->method),
		          url.ptr ? url.ptr : "<invalid>");
		git_str_dispose(&url);
	}

	if ((error = http_client_connect(client, request)) < 0 ||
	    (error = generate_request(client, request)) < 0 ||
	    (error = client_write_request(client)) < 0)
		goto done;

	client->state = SENT_REQUEST;

	if (request->expect_continue) {
		if ((error = git_http_client_read_response(&response, client)) < 0 ||
		    (error = git_http_client_skip_body(client)) < 0)
			goto done;

		error = 0;

		if (response.status != GIT_HTTP_STATUS_CONTINUE) {
			save_early_response(client, &response);
			goto done;
		}
	}

	if (request->content_length || request->chunked) {
		client->state = SENDING_BODY;
		client->request_body_len = request->content_length;
		client->request_body_remain = request->content_length;
		client->request_chunked = request->chunked;
	}

	reset_parser(client);

done:
	if (error == GIT_RETRY)
		error = 0;

	git_http_response_dispose(&response);
	return error;
}

bool git_http_client_has_response(git_http_client *client)
{
	return (client->state == HAS_EARLY_RESPONSE ||
	        client->state > SENT_REQUEST);
}

int git_http_client_send_body(
	git_http_client *client,
	const char *buffer,
	size_t buffer_len)
{
	git_http_server *server;
	git_str hdr = GIT_STR_INIT;
	int error;

	GIT_ASSERT_ARG(client);

	/* If we're waiting for proxy auth, don't sending more requests. */
	if (client->state == HAS_EARLY_RESPONSE)
		return 0;

	if (client->state != SENDING_BODY) {
		git_error_set(GIT_ERROR_HTTP, "client is in invalid state");
		return -1;
	}

	if (!buffer_len)
		return 0;

	server = &client->server;

	if (client->request_body_len) {
		GIT_ASSERT(buffer_len <= client->request_body_remain);

		if ((error = stream_write(server, buffer, buffer_len)) < 0)
			goto done;

		client->request_body_remain -= buffer_len;
	} else {
		if ((error = git_str_printf(&hdr, "%" PRIxZ "\r\n", buffer_len)) < 0 ||
		    (error = stream_write(server, hdr.ptr, hdr.size)) < 0 ||
		    (error = stream_write(server, buffer, buffer_len)) < 0 ||
		    (error = stream_write(server, "\r\n", 2)) < 0)
			goto done;
	}

done:
	git_str_dispose(&hdr);
	return error;
}

static int complete_request(git_http_client *client)
{
	int error = 0;

	GIT_ASSERT_ARG(client);
	GIT_ASSERT(client->state == SENDING_BODY);

	if (client->request_body_len && client->request_body_remain) {
		git_error_set(GIT_ERROR_HTTP, "truncated write");
		error = -1;
	} else if (client->request_chunked) {
		error = stream_write(&client->server, "0\r\n\r\n", 5);
	}

	client->state = SENT_REQUEST;
	return error;
}

int git_http_client_read_response(
	git_http_response *response,
	git_http_client *client)
{
	http_parser_context parser_context = {0};
	int error;

	GIT_ASSERT_ARG(response);
	GIT_ASSERT_ARG(client);

	if (client->state == SENDING_BODY) {
		if ((error = complete_request(client)) < 0)
			goto done;
	}

	if (client->state == HAS_EARLY_RESPONSE) {
		memcpy(response, &client->early_response, sizeof(git_http_response));
		memset(&client->early_response, 0, sizeof(git_http_response));
		client->state = DONE;
		return 0;
	}

	if (client->state != SENT_REQUEST) {
		git_error_set(GIT_ERROR_HTTP, "client is in invalid state");
		error = -1;
		goto done;
	}

	git_http_response_dispose(response);

	if (client->current_server == PROXY) {
		git_vector_dispose_deep(&client->proxy.auth_challenges);
	} else if(client->current_server == SERVER) {
		git_vector_dispose_deep(&client->server.auth_challenges);
	}

	client->state = READING_RESPONSE;
	client->keepalive = 0;
	client->parser.data = &parser_context;

	parser_context.client = client;
	parser_context.response = response;

	while (client->state == READING_RESPONSE) {
		if ((error = client_read_and_parse(client)) < 0)
			goto done;
	}

	GIT_ASSERT(client->state == READING_BODY || client->state == DONE);

done:
	git_str_dispose(&parser_context.parse_header_name);
	git_str_dispose(&parser_context.parse_header_value);
	client->parser.data = NULL;

	return error;
}

int git_http_client_read_body(
	git_http_client *client,
	char *buffer,
	size_t buffer_size)
{
	http_parser_context parser_context = {0};
	int error = 0;

	if (client->state == DONE)
		return 0;

	if (client->state != READING_BODY) {
		git_error_set(GIT_ERROR_HTTP, "client is in invalid state");
		return -1;
	}

	/*
	 * Now we'll read from the socket and http_parser will pipeline the
	 * data directly to the client.
	 */

	parser_context.client = client;
	parser_context.output_buf = buffer;
	parser_context.output_size = buffer_size;

	client->parser.data = &parser_context;

	/*
	 * Clients expect to get a non-zero amount of data from us,
	 * so we either block until we have data to return, until we
	 * hit EOF or there's an error.  Do this in a loop, since we
	 * may end up reading only some stream metadata (like chunk
	 * information).
	 */
	while (!parser_context.output_written) {
		error = client_read_and_parse(client);

		if (error <= 0)
			goto done;

		if (client->state == DONE)
			break;
	}

	GIT_ASSERT(parser_context.output_written <= INT_MAX);
	error = (int)parser_context.output_written;

done:
	if (error < 0)
		client->connected = 0;

	client->parser.data = NULL;

	return error;
}

int git_http_client_skip_body(git_http_client *client)
{
	http_parser_context parser_context = {0};
	int error;

	if (client->state == DONE)
		return 0;

	if (client->state != READING_BODY) {
		git_error_set(GIT_ERROR_HTTP, "client is in invalid state");
		return -1;
	}

	parser_context.client = client;
	client->parser.data = &parser_context;

	do {
		error = client_read_and_parse(client);

		if (parser_context.error != GIT_HTTP_PARSER_OK ||
		    (parser_context.parse_status != PARSE_STATUS_OK &&
		     parser_context.parse_status != PARSE_STATUS_NO_OUTPUT)) {
			git_error_set(GIT_ERROR_HTTP,
			              "unexpected data handled in callback");
			error = -1;
		}
	} while (error >= 0 && client->state != DONE);

	if (error < 0)
		client->connected = 0;

	client->parser.data = NULL;

	return error;
}

/*
 * Create an http_client capable of communicating with the given remote
 * host.
 */
int git_http_client_new(
	git_http_client **out,
	git_http_client_options *opts)
{
	git_http_client *client;

	GIT_ASSERT_ARG(out);

	client = git__calloc(1, sizeof(git_http_client));
	GIT_ERROR_CHECK_ALLOC(client);

	git_str_init(&client->read_buf, GIT_READ_BUFFER_SIZE);
	GIT_ERROR_CHECK_ALLOC(client->read_buf.ptr);

	if (opts)
		memcpy(&client->opts, opts, sizeof(git_http_client_options));

	*out = client;
	return 0;
}

/* Update the options of an existing httpclient instance. */
void git_http_client_set_options(
	git_http_client *client,
	git_http_client_options *opts)
{
	if (opts)
		memcpy(&client->opts, opts, sizeof(git_http_client_options));
}

GIT_INLINE(void) http_server_close(git_http_server *server)
{
	if (server->stream) {
		git_stream_close(server->stream);
		git_stream_free(server->stream);
		server->stream = NULL;
	}

	git_net_url_dispose(&server->url);

	git_vector_dispose_deep(&server->auth_challenges);
	free_auth_context(server);
}

static void http_client_close(git_http_client *client)
{
	http_server_close(&client->server);
	http_server_close(&client->proxy);

	git_str_dispose(&client->request_msg);

	client->state = 0;
	client->request_count = 0;
	client->connected = 0;
	client->keepalive = 0;
}

void git_http_client_free(git_http_client *client)
{
	if (!client)
		return;

	http_client_close(client);
	git_str_dispose(&client->read_buf);
	git__free(client);
}
