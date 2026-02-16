/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_transports_httpclient_h__
#define INCLUDE_transports_httpclient_h__

#include "common.h"
#include "net.h"

#define GIT_HTTP_STATUS_CONTINUE                      100
#define GIT_HTTP_STATUS_OK                            200
#define GIT_HTTP_MOVED_PERMANENTLY                    301
#define GIT_HTTP_FOUND                                302
#define GIT_HTTP_SEE_OTHER                            303
#define GIT_HTTP_TEMPORARY_REDIRECT                   307
#define GIT_HTTP_PERMANENT_REDIRECT                   308
#define GIT_HTTP_STATUS_UNAUTHORIZED                  401
#define GIT_HTTP_STATUS_PROXY_AUTHENTICATION_REQUIRED 407

typedef struct git_http_client git_http_client;

/** Method for the HTTP request */
typedef enum {
	GIT_HTTP_METHOD_GET,
	GIT_HTTP_METHOD_POST,
	GIT_HTTP_METHOD_CONNECT
} git_http_method;

/** An HTTP request */
typedef struct {
	git_http_method method;            /**< Method for the request */
	git_net_url *url;                  /**< Full request URL */
	git_net_url *proxy;                /**< Proxy to use */

	/* Headers */
	const char *accept;                /**< Contents of the Accept header */
	const char *content_type;          /**< Content-Type header (for POST) */
	git_credential *credentials;       /**< Credentials to authenticate with */
	git_credential *proxy_credentials; /**< Credentials for proxy */
	git_strarray *custom_headers;      /**< Additional headers to deliver */

	/* To POST a payload, either set content_length OR set chunked. */
	size_t content_length;             /**< Length of the POST body */
	unsigned chunked : 1,              /**< Post with chunking */
	         expect_continue : 1;      /**< Use expect/continue negotiation */
} git_http_request;

typedef struct {
	int status;

	/* Headers */
	char *content_type;
	size_t content_length;
	char *location;

	/* Authentication headers */
	unsigned server_auth_schemetypes; /**< Schemes requested by remote */
	unsigned server_auth_credtypes;   /**< Supported cred types for remote */

	unsigned proxy_auth_schemetypes;  /**< Schemes requested by proxy */
	unsigned proxy_auth_credtypes;    /**< Supported cred types for proxy */

	unsigned chunked : 1,             /**< Response body is chunked */
	         resend_credentials : 1;  /**< Resend with authentication */
} git_http_response;

typedef struct {
	/** Certificate check callback for the remote */
	git_transport_certificate_check_cb server_certificate_check_cb;
	void *server_certificate_check_payload;

	/** Certificate check callback for the proxy */
	git_transport_certificate_check_cb proxy_certificate_check_cb;
	void *proxy_certificate_check_payload;
} git_http_client_options;

/**
 * Create a new httpclient instance with the given options.
 *
 * @param out pointer to receive the new instance
 * @param opts options to create the client with or NULL for defaults
 */
extern int git_http_client_new(
	git_http_client **out,
	git_http_client_options *opts);

/**
 * Update the options of an existing httpclient instance.
 *
 * @param client the httpclient instance to modify
 * @param opts new options or NULL to keep existing options
 */
extern void git_http_client_set_options(
	git_http_client *client,
	git_http_client_options *opts);

/*
 * Sends a request to the host specified by the request URL.  If the
 * method is POST, either the content_length or the chunked flag must
 * be specified.  The body should be provided in subsequent calls to
 * git_http_client_send_body.
 *
 * @param client the client to write the request to
 * @param request the request to send
 */
extern int git_http_client_send_request(
	git_http_client *client,
	git_http_request *request);

/*
 * After sending a request, there may already be a response to read --
 * either because there was a non-continue response to an expect: continue
 * request, or because the server pipelined a response to us before we even
 * sent the request.  Examine the state.
 *
 * @param client the client to examine
 * @return true if there's already a response to read, false otherwise
 */
extern bool git_http_client_has_response(git_http_client *client);

/**
 * Sends the given buffer to the remote as part of the request body.  The
 * request must have specified either a content_length or the chunked flag.
 *
 * @param client the client to write the request body to
 * @param buffer the request body
 * @param buffer_len number of bytes of the buffer to send
 */
extern int git_http_client_send_body(
	git_http_client *client,
	const char *buffer,
	size_t buffer_len);

/**
 * Reads the headers of a response to a request.  This will consume the
 * entirety of the headers of a response from the server.  The body (if any)
 * can be read by calling git_http_client_read_body.  Callers must free
 * the response with git_http_response_dispose.
 *
 * @param response pointer to the response object to fill
 * @param client the client to read the response from
 */
extern int git_http_client_read_response(
	git_http_response *response,
	git_http_client *client);

/**
 * Reads some or all of the body of a response.  At most buffer_size (or
 * INT_MAX) bytes will be read and placed into the buffer provided.  The
 * number of bytes read will be returned, or 0 to indicate that the end of
 * the body has been read.
 *
 * @param client the client to read the response from
 * @param buffer pointer to the buffer to fill
 * @param buffer_size the maximum number of bytes to read
 * @return the number of bytes read, 0 on end of body, or error code
 */
extern int git_http_client_read_body(
	git_http_client *client,
	char *buffer,
	size_t buffer_size);

/**
 * Reads all of the (remainder of the) body of the response and ignores it.
 * None of the data from the body will be returned to the caller.
 *
 * @param client the client to read the response from
 * @return 0 or an error code
 */
extern int git_http_client_skip_body(git_http_client *client);

/**
 * Examines the status code of the response to determine if it is a
 * redirect of any type (eg, 301, 302, etc).
 *
 * @param response the response to inspect
 * @return true if the response is a redirect, false otherwise
 */
extern bool git_http_response_is_redirect(git_http_response *response);

/**
 * Frees any memory associated with the response.
 *
 * @param response the response to free
 */
extern void git_http_response_dispose(git_http_response *response);

/**
 * Frees any memory associated with the client.  If any sockets are open,
 * they will be closed.
 *
 * @param client the client to free
 */
extern void git_http_client_free(git_http_client *client);

#endif
