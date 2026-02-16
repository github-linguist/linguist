/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#ifdef GIT_WINHTTP

#include "git2.h"
#include "git2/transport.h"
#include "posix.h"
#include "str.h"
#include "smart.h"
#include "remote.h"
#include "repository.h"
#include "http.h"
#include "git2/sys/credential.h"

#include <wincrypt.h>
#include <winhttp.h>

/* For IInternetSecurityManager zone check */
#include <objbase.h>
#include <urlmon.h>

#define WIDEN2(s) L ## s
#define WIDEN(s) WIDEN2(s)

#define MAX_CONTENT_TYPE_LEN	100
#define WINHTTP_OPTION_PEERDIST_EXTENSION_STATE	109
#define CACHED_POST_BODY_BUF_SIZE	4096
#define UUID_LENGTH_CCH	32
#define TIMEOUT_INFINITE -1
#define DEFAULT_CONNECT_TIMEOUT 60000
#ifndef WINHTTP_IGNORE_REQUEST_TOTAL_LENGTH
#define WINHTTP_IGNORE_REQUEST_TOTAL_LENGTH 0
#endif

#ifndef WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1
# define WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1 0x00000200
#endif

#ifndef WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2
# define WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2 0x00000800
#endif

#ifndef WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_3
# define WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_3 0x00002000
#endif

#ifndef WINHTTP_NO_CLIENT_CERT_CONTEXT
# define WINHTTP_NO_CLIENT_CERT_CONTEXT NULL
#endif

#ifndef HTTP_STATUS_PERMANENT_REDIRECT
# define HTTP_STATUS_PERMANENT_REDIRECT 308
#endif

#ifndef DWORD_MAX
# define DWORD_MAX 0xffffffff
#endif

bool git_http__expect_continue = false;

static const char *prefix_https = "https://";
static const char *upload_pack_service = "upload-pack";
static const char *upload_pack_ls_service_url = "/info/refs?service=git-upload-pack";
static const char *upload_pack_service_url = "/git-upload-pack";
static const char *receive_pack_service = "receive-pack";
static const char *receive_pack_ls_service_url = "/info/refs?service=git-receive-pack";
static const char *receive_pack_service_url = "/git-receive-pack";
static const wchar_t *get_verb = L"GET";
static const wchar_t *post_verb = L"POST";
static const wchar_t *pragma_nocache = L"Pragma: no-cache";
static const wchar_t *transfer_encoding = L"Transfer-Encoding: chunked";
static const int no_check_cert_flags = SECURITY_FLAG_IGNORE_CERT_CN_INVALID |
	SECURITY_FLAG_IGNORE_CERT_DATE_INVALID |
	SECURITY_FLAG_IGNORE_UNKNOWN_CA;

#if defined(__MINGW32__)
static const CLSID CLSID_InternetSecurityManager_mingw =
	{ 0x7B8A2D94, 0x0AC9, 0x11D1,
	{ 0x89, 0x6C, 0x00, 0xC0, 0x4F, 0xB6, 0xBF, 0xC4 } };
static const IID IID_IInternetSecurityManager_mingw =
	{ 0x79EAC9EE, 0xBAF9, 0x11CE,
	{ 0x8C, 0x82, 0x00, 0xAA, 0x00, 0x4B, 0xA9, 0x0B } };

# define CLSID_InternetSecurityManager CLSID_InternetSecurityManager_mingw
# define IID_IInternetSecurityManager IID_IInternetSecurityManager_mingw
#endif

#define OWNING_SUBTRANSPORT(s) ((winhttp_subtransport *)(s)->parent.subtransport)

typedef enum {
	GIT_WINHTTP_AUTH_BASIC = 1,
	GIT_WINHTTP_AUTH_NTLM = 2,
	GIT_WINHTTP_AUTH_NEGOTIATE = 4,
	GIT_WINHTTP_AUTH_DIGEST = 8
} winhttp_authmechanism_t;

typedef struct {
	git_smart_subtransport_stream parent;
	const char *service;
	const char *service_url;
	const wchar_t *verb;
	HINTERNET request;
	wchar_t *request_uri;
	char *chunk_buffer;
	unsigned chunk_buffer_len;
	HANDLE post_body;
	DWORD post_body_len;
	unsigned sent_request : 1,
		received_response : 1,
		chunked : 1,
		status_sending_request_reached: 1;
} winhttp_stream;

typedef struct {
	git_net_url url;
	git_credential *cred;
	int auth_mechanisms;
	bool url_cred_presented;
} winhttp_server;

typedef struct {
	git_smart_subtransport parent;
	transport_smart *owner;

	winhttp_server server;
	winhttp_server proxy;

	HINTERNET session;
	HINTERNET connection;
} winhttp_subtransport;

static int apply_userpass_credentials(HINTERNET request, DWORD target, int mechanisms, git_credential *cred)
{
	git_credential_userpass_plaintext *c = (git_credential_userpass_plaintext *)cred;
	wchar_t *user = NULL, *pass = NULL;
	int user_len = 0, pass_len = 0, error = 0;
	DWORD native_scheme;

	if (mechanisms & GIT_WINHTTP_AUTH_NEGOTIATE) {
		native_scheme = WINHTTP_AUTH_SCHEME_NEGOTIATE;
	} else if (mechanisms & GIT_WINHTTP_AUTH_NTLM) {
		native_scheme = WINHTTP_AUTH_SCHEME_NTLM;
	} else if (mechanisms & GIT_WINHTTP_AUTH_DIGEST) {
		native_scheme = WINHTTP_AUTH_SCHEME_DIGEST;
	} else if (mechanisms & GIT_WINHTTP_AUTH_BASIC) {
		native_scheme = WINHTTP_AUTH_SCHEME_BASIC;
	} else {
		git_error_set(GIT_ERROR_HTTP, "invalid authentication scheme");
		error = GIT_EAUTH;
		goto done;
	}

	if ((error = user_len = git_utf8_to_16_alloc(&user, c->username)) < 0)
		goto done;

	if ((error = pass_len = git_utf8_to_16_alloc(&pass, c->password)) < 0)
		goto done;

	if (!WinHttpSetCredentials(request, target, native_scheme, user, pass, NULL)) {
		git_error_set(GIT_ERROR_OS, "failed to set credentials");
		error = -1;
	}

done:
	if (user_len > 0)
		git__memzero(user, user_len * sizeof(wchar_t));

	if (pass_len > 0)
		git__memzero(pass, pass_len * sizeof(wchar_t));

	git__free(user);
	git__free(pass);

	return error;
}

static int apply_default_credentials(HINTERNET request, DWORD target, int mechanisms)
{
	DWORD autologon_level = WINHTTP_AUTOLOGON_SECURITY_LEVEL_LOW;
	DWORD native_scheme = 0;

	if ((mechanisms & GIT_WINHTTP_AUTH_NEGOTIATE) != 0) {
		native_scheme = WINHTTP_AUTH_SCHEME_NEGOTIATE;
	} else if ((mechanisms & GIT_WINHTTP_AUTH_NTLM) != 0) {
		native_scheme = WINHTTP_AUTH_SCHEME_NTLM;
	} else {
		git_error_set(GIT_ERROR_HTTP, "invalid authentication scheme");
		return GIT_EAUTH;
	}

	/*
	 * Autologon policy must be "low" to use default creds.
	 * This is safe as the user has explicitly requested it.
	 */
	if (!WinHttpSetOption(request, WINHTTP_OPTION_AUTOLOGON_POLICY, &autologon_level, sizeof(DWORD))) {
		git_error_set(GIT_ERROR_OS, "could not configure logon policy");
		return -1;
	}

	if (!WinHttpSetCredentials(request, target, native_scheme, NULL, NULL, NULL)) {
		git_error_set(GIT_ERROR_OS, "could not configure credentials");
		return -1;
	}

	return 0;
}

static int acquire_url_cred(
	git_credential **cred,
	unsigned int allowed_types,
	const char *username,
	const char *password)
{
	if (allowed_types & GIT_CREDENTIAL_USERPASS_PLAINTEXT)
		return git_credential_userpass_plaintext_new(cred, username, password);

	if ((allowed_types & GIT_CREDENTIAL_DEFAULT) && *username == '\0' && *password == '\0')
		return git_credential_default_new(cred);

	return 1;
}

static int acquire_fallback_cred(
	git_credential **cred,
	const char *url,
	unsigned int allowed_types)
{
	int error = 1;

	/* If the target URI supports integrated Windows authentication
	 * as an authentication mechanism */
	if (GIT_CREDENTIAL_DEFAULT & allowed_types) {
		wchar_t *wide_url;
		HRESULT hCoInitResult;

		/* Convert URL to wide characters */
		if (git_utf8_to_16_alloc(&wide_url, url) < 0) {
			git_error_set(GIT_ERROR_OS, "failed to convert string to wide form");
			return -1;
		}

		hCoInitResult = CoInitializeEx(NULL, COINIT_MULTITHREADED);

		if (SUCCEEDED(hCoInitResult) || hCoInitResult == RPC_E_CHANGED_MODE) {
			IInternetSecurityManager *pISM;

			/* And if the target URI is in the My Computer, Intranet, or Trusted zones */
			if (SUCCEEDED(CoCreateInstance(&CLSID_InternetSecurityManager, NULL,
				CLSCTX_ALL, &IID_IInternetSecurityManager, (void **)&pISM))) {
				DWORD dwZone;

				if (SUCCEEDED(pISM->lpVtbl->MapUrlToZone(pISM, wide_url, &dwZone, 0)) &&
					(URLZONE_LOCAL_MACHINE == dwZone ||
					URLZONE_INTRANET == dwZone ||
					URLZONE_TRUSTED == dwZone)) {
					git_credential *existing = *cred;

					if (existing)
						existing->free(existing);

					/* Then use default Windows credentials to authenticate this request */
					error = git_credential_default_new(cred);
				}

				pISM->lpVtbl->Release(pISM);
			}

			/* Only uninitialize if the call to CoInitializeEx was successful. */
			if (SUCCEEDED(hCoInitResult))
				CoUninitialize();
		}

		git__free(wide_url);
	}

	return error;
}

static int certificate_check(winhttp_stream *s, int valid)
{
	int error;
	winhttp_subtransport *t = OWNING_SUBTRANSPORT(s);
	PCERT_CONTEXT cert_ctx;
	DWORD cert_ctx_size = sizeof(cert_ctx);
	git_cert_x509 cert;

	/* If there is no override, we should fail if WinHTTP doesn't think it's fine */
	if (t->owner->connect_opts.callbacks.certificate_check == NULL && !valid) {
		if (git_error_last()->klass == GIT_ERROR_NONE)
			git_error_set(GIT_ERROR_HTTP, "unknown certificate check failure");

		return GIT_ECERTIFICATE;
	}

	if (t->owner->connect_opts.callbacks.certificate_check == NULL || git__strcmp(t->server.url.scheme, "https") != 0)
		return 0;

	if (!WinHttpQueryOption(s->request, WINHTTP_OPTION_SERVER_CERT_CONTEXT, &cert_ctx, &cert_ctx_size)) {
		git_error_set(GIT_ERROR_OS, "failed to get server certificate");
		return -1;
	}

	git_error_clear();
	cert.parent.cert_type = GIT_CERT_X509;
	cert.data = cert_ctx->pbCertEncoded;
	cert.len = cert_ctx->cbCertEncoded;
	error = t->owner->connect_opts.callbacks.certificate_check((git_cert *) &cert, valid, t->server.url.host, t->owner->connect_opts.callbacks.payload);
	CertFreeCertificateContext(cert_ctx);

	if (error == GIT_PASSTHROUGH)
		error = valid ? 0 : GIT_ECERTIFICATE;

	if (error < 0 && git_error_last()->klass == GIT_ERROR_NONE)
		git_error_set(GIT_ERROR_HTTP, "user cancelled certificate check");

	return error;
}

static void winhttp_stream_close(winhttp_stream *s)
{
	if (s->chunk_buffer) {
		git__free(s->chunk_buffer);
		s->chunk_buffer = NULL;
	}

	if (s->post_body) {
		CloseHandle(s->post_body);
		s->post_body = NULL;
	}

	if (s->request_uri) {
		git__free(s->request_uri);
		s->request_uri = NULL;
	}

	if (s->request) {
		WinHttpCloseHandle(s->request);
		s->request = NULL;
	}

	s->sent_request = 0;
}

static int apply_credentials(
	HINTERNET request,
	git_net_url *url,
	int target,
	git_credential *creds,
	int mechanisms)
{
	int error = 0;

	GIT_UNUSED(url);

	/* If we have creds, just apply them */
	if (creds && creds->credtype == GIT_CREDENTIAL_USERPASS_PLAINTEXT)
		error = apply_userpass_credentials(request, target, mechanisms, creds);
	else if (creds && creds->credtype == GIT_CREDENTIAL_DEFAULT)
		error = apply_default_credentials(request, target, mechanisms);

	return error;
}

static int winhttp_stream_connect(winhttp_stream *s)
{
	winhttp_subtransport *t = OWNING_SUBTRANSPORT(s);
	git_str buf = GIT_STR_INIT;
	char *proxy_url = NULL;
	wchar_t ct[MAX_CONTENT_TYPE_LEN];
	LPCWSTR types[] = { L"*/*", NULL };
	BOOL peerdist = FALSE;
	int error = -1;
	unsigned long disable_redirects = WINHTTP_DISABLE_REDIRECTS;
	int default_timeout = TIMEOUT_INFINITE;
	int default_connect_timeout = DEFAULT_CONNECT_TIMEOUT;
	DWORD autologon_policy = WINHTTP_AUTOLOGON_SECURITY_LEVEL_HIGH;

	const char *service_url = s->service_url;
	size_t i;
	const git_proxy_options *proxy_opts;

	/* If path already ends in /, remove the leading slash from service_url */
	if ((git__suffixcmp(t->server.url.path, "/") == 0) && (git__prefixcmp(service_url, "/") == 0))
		service_url++;
	/* Prepare URL */
	git_str_printf(&buf, "%s%s", t->server.url.path, service_url);

	if (git_str_oom(&buf))
		return -1;

	/* Convert URL to wide characters */
	if (git_utf8_to_16_alloc(&s->request_uri, git_str_cstr(&buf)) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to convert string to wide form");
		goto on_error;
	}

	/* Establish request */
	s->request = WinHttpOpenRequest(
			t->connection,
			s->verb,
			s->request_uri,
			NULL,
			WINHTTP_NO_REFERER,
			types,
			git__strcmp(t->server.url.scheme, "https") == 0 ? WINHTTP_FLAG_SECURE : 0);

	if (!s->request) {
		git_error_set(GIT_ERROR_OS, "failed to open request");
		goto on_error;
	}

	/* Never attempt default credentials; we'll provide them explicitly. */
	if (!WinHttpSetOption(s->request, WINHTTP_OPTION_AUTOLOGON_POLICY, &autologon_policy, sizeof(DWORD)))
		return -1;

	if (!WinHttpSetTimeouts(s->request, default_timeout, default_connect_timeout, default_timeout, default_timeout)) {
		git_error_set(GIT_ERROR_OS, "failed to set timeouts for WinHTTP");
		goto on_error;
	}

	proxy_opts = &t->owner->connect_opts.proxy_opts;
	if (proxy_opts->type == GIT_PROXY_AUTO) {
		/* Set proxy if necessary */
		if (git_remote__http_proxy(&proxy_url, t->owner->owner, &t->server.url) < 0)
			goto on_error;
	}
	else if (proxy_opts->type == GIT_PROXY_SPECIFIED) {
		proxy_url = git__strdup(proxy_opts->url);
		GIT_ERROR_CHECK_ALLOC(proxy_url);
	}

	if (proxy_url && *proxy_url) {
		git_str processed_url = GIT_STR_INIT;
		WINHTTP_PROXY_INFO proxy_info;
		wchar_t *proxy_wide;

		git_net_url_dispose(&t->proxy.url);

		if ((error = git_net_url_parse_http(&t->proxy.url, proxy_url)) < 0)
			goto on_error;

		if (!git_net_url_valid(&t->proxy.url)) {
			git_error_set(GIT_ERROR_HTTP, "invalid URL: '%s'", proxy_url);
			error = -1;
			goto on_error;
		}

		git_str_puts(&processed_url, t->proxy.url.scheme);
		git_str_PUTS(&processed_url, "://");

		if (git_net_url_is_ipv6(&t->proxy.url))
			git_str_putc(&processed_url, '[');

		git_str_puts(&processed_url, t->proxy.url.host);

		if (git_net_url_is_ipv6(&t->proxy.url))
			git_str_putc(&processed_url, ']');

		if (!git_net_url_is_default_port(&t->proxy.url))
			git_str_printf(&processed_url, ":%s", t->proxy.url.port);

		if (git_str_oom(&processed_url)) {
			error = -1;
			goto on_error;
		}

		/* Convert URL to wide characters */
		error = git_utf8_to_16_alloc(&proxy_wide, processed_url.ptr);
		git_str_dispose(&processed_url);
		if (error < 0)
			goto on_error;

		proxy_info.dwAccessType = WINHTTP_ACCESS_TYPE_NAMED_PROXY;
		proxy_info.lpszProxy = proxy_wide;
		proxy_info.lpszProxyBypass = NULL;

		if (!WinHttpSetOption(s->request,
			WINHTTP_OPTION_PROXY,
			&proxy_info,
			sizeof(WINHTTP_PROXY_INFO))) {
			git_error_set(GIT_ERROR_OS, "failed to set proxy");
			git__free(proxy_wide);
			goto on_error;
		}

		git__free(proxy_wide);

		if ((error = apply_credentials(s->request, &t->proxy.url, WINHTTP_AUTH_TARGET_PROXY, t->proxy.cred, t->proxy.auth_mechanisms)) < 0)
			goto on_error;
	}

	/* Disable WinHTTP redirects so we can handle them manually. Why, you ask?
	 * http://social.msdn.microsoft.com/Forums/windowsdesktop/en-US/b2ff8879-ab9f-4218-8f09-16d25dff87ae
	 */
	if (!WinHttpSetOption(s->request,
		WINHTTP_OPTION_DISABLE_FEATURE,
		&disable_redirects,
		sizeof(disable_redirects))) {
			git_error_set(GIT_ERROR_OS, "failed to disable redirects");
			error = -1;
			goto on_error;
	}

	/* Strip unwanted headers (X-P2P-PeerDist, X-P2P-PeerDistEx) that WinHTTP
	 * adds itself. This option may not be supported by the underlying
	 * platform, so we do not error-check it */
	WinHttpSetOption(s->request,
		WINHTTP_OPTION_PEERDIST_EXTENSION_STATE,
		&peerdist,
		sizeof(peerdist));

	/* Send Pragma: no-cache header */
	if (!WinHttpAddRequestHeaders(s->request, pragma_nocache, (ULONG) -1L, WINHTTP_ADDREQ_FLAG_ADD)) {
		git_error_set(GIT_ERROR_OS, "failed to add a header to the request");
		goto on_error;
	}

	if (post_verb == s->verb) {
		/* Send Content-Type and Accept headers -- only necessary on a POST */
		git_str_clear(&buf);
		if (git_str_printf(&buf,
			"Content-Type: application/x-git-%s-request",
			s->service) < 0)
			goto on_error;

		if (git_utf8_to_16(ct, MAX_CONTENT_TYPE_LEN, git_str_cstr(&buf)) < 0) {
			git_error_set(GIT_ERROR_OS, "failed to convert content-type to wide characters");
			goto on_error;
		}

		if (!WinHttpAddRequestHeaders(s->request, ct, (ULONG)-1L,
			WINHTTP_ADDREQ_FLAG_ADD | WINHTTP_ADDREQ_FLAG_REPLACE)) {
			git_error_set(GIT_ERROR_OS, "failed to add a header to the request");
			goto on_error;
		}

		git_str_clear(&buf);
		if (git_str_printf(&buf,
			"Accept: application/x-git-%s-result",
			s->service) < 0)
			goto on_error;

		if (git_utf8_to_16(ct, MAX_CONTENT_TYPE_LEN, git_str_cstr(&buf)) < 0) {
			git_error_set(GIT_ERROR_OS, "failed to convert accept header to wide characters");
			goto on_error;
		}

		if (!WinHttpAddRequestHeaders(s->request, ct, (ULONG)-1L,
			WINHTTP_ADDREQ_FLAG_ADD | WINHTTP_ADDREQ_FLAG_REPLACE)) {
			git_error_set(GIT_ERROR_OS, "failed to add a header to the request");
			goto on_error;
		}
	}

	for (i = 0; i < t->owner->connect_opts.custom_headers.count; i++) {
		if (t->owner->connect_opts.custom_headers.strings[i]) {
			wchar_t *custom_header_wide = NULL;

			git_str_clear(&buf);
			git_str_puts(&buf, t->owner->connect_opts.custom_headers.strings[i]);

			/* Convert header to wide characters */
			if ((error = git_utf8_to_16_alloc(&custom_header_wide, git_str_cstr(&buf))) < 0)
				goto on_error;

			if (!WinHttpAddRequestHeaders(s->request, custom_header_wide, (ULONG)-1L,
				WINHTTP_ADDREQ_FLAG_ADD | WINHTTP_ADDREQ_FLAG_REPLACE)) {
				git_error_set(GIT_ERROR_OS, "failed to add a header to the request");
				git__free(custom_header_wide);
				goto on_error;
			}

			git__free(custom_header_wide);
		}
	}

	if ((error = apply_credentials(s->request, &t->server.url, WINHTTP_AUTH_TARGET_SERVER, t->server.cred, t->server.auth_mechanisms)) < 0)
		goto on_error;

	/* We've done everything up to calling WinHttpSendRequest. */

	error = 0;

on_error:
	if (error < 0)
		winhttp_stream_close(s);

	git__free(proxy_url);
	git_str_dispose(&buf);
	return error;
}

static int parse_unauthorized_response(
	int *allowed_types,
	int *allowed_mechanisms,
	HINTERNET request)
{
	DWORD supported, first, target;

	*allowed_types = 0;
	*allowed_mechanisms = 0;

	/* WinHttpQueryHeaders() must be called before WinHttpQueryAuthSchemes().
	 * We can assume this was already done, since we know we are unauthorized.
	 */
	if (!WinHttpQueryAuthSchemes(request, &supported, &first, &target)) {
		git_error_set(GIT_ERROR_OS, "failed to parse supported auth schemes");
		return GIT_EAUTH;
	}

	if (WINHTTP_AUTH_SCHEME_NTLM & supported) {
		*allowed_types |= GIT_CREDENTIAL_USERPASS_PLAINTEXT;
		*allowed_types |= GIT_CREDENTIAL_DEFAULT;
		*allowed_mechanisms |= GIT_WINHTTP_AUTH_NTLM;
	}

	if (WINHTTP_AUTH_SCHEME_NEGOTIATE & supported) {
		*allowed_types |= GIT_CREDENTIAL_DEFAULT;
		*allowed_mechanisms |= GIT_WINHTTP_AUTH_NEGOTIATE;
	}

	if (WINHTTP_AUTH_SCHEME_BASIC & supported) {
		*allowed_types |= GIT_CREDENTIAL_USERPASS_PLAINTEXT;
		*allowed_mechanisms |= GIT_WINHTTP_AUTH_BASIC;
	}

	if (WINHTTP_AUTH_SCHEME_DIGEST & supported) {
		*allowed_types |= GIT_CREDENTIAL_USERPASS_PLAINTEXT;
		*allowed_mechanisms |= GIT_WINHTTP_AUTH_DIGEST;
	}

	return 0;
}

static int write_chunk(HINTERNET request, const char *buffer, size_t len)
{
	DWORD bytes_written;
	git_str buf = GIT_STR_INIT;

	/* Chunk header */
	git_str_printf(&buf, "%"PRIXZ"\r\n", len);

	if (git_str_oom(&buf))
		return -1;

	if (!WinHttpWriteData(request,
		git_str_cstr(&buf),	(DWORD)git_str_len(&buf),
		&bytes_written)) {
		git_str_dispose(&buf);
		git_error_set(GIT_ERROR_OS, "failed to write chunk header");
		return -1;
	}

	git_str_dispose(&buf);

	/* Chunk body */
	if (!WinHttpWriteData(request,
		buffer, (DWORD)len,
		&bytes_written)) {
		git_error_set(GIT_ERROR_OS, "failed to write chunk");
		return -1;
	}

	/* Chunk footer */
	if (!WinHttpWriteData(request,
		"\r\n", 2,
		&bytes_written)) {
		git_error_set(GIT_ERROR_OS, "failed to write chunk footer");
		return -1;
	}

	return 0;
}

static int winhttp_close_connection(winhttp_subtransport *t)
{
	int ret = 0;

	if (t->connection) {
		if (!WinHttpCloseHandle(t->connection)) {
			git_error_set(GIT_ERROR_OS, "unable to close connection");
			ret = -1;
		}

		t->connection = NULL;
	}

	if (t->session) {
		if (!WinHttpCloseHandle(t->session)) {
			git_error_set(GIT_ERROR_OS, "unable to close session");
			ret = -1;
		}

		t->session = NULL;
	}

	return ret;
}

static void CALLBACK winhttp_status(
	HINTERNET connection,
	DWORD_PTR ctx,
	DWORD code,
	LPVOID info,
	DWORD info_len)
{
	DWORD status;

	GIT_UNUSED(connection);
	GIT_UNUSED(info_len);

	switch (code) {
		case WINHTTP_CALLBACK_STATUS_SECURE_FAILURE:
			status = *((DWORD *)info);

			if ((status & WINHTTP_CALLBACK_STATUS_FLAG_CERT_CN_INVALID))
				git_error_set(GIT_ERROR_HTTP, "SSL certificate issued for different common name");
			else if ((status & WINHTTP_CALLBACK_STATUS_FLAG_CERT_DATE_INVALID))
				git_error_set(GIT_ERROR_HTTP, "SSL certificate has expired");
			else if ((status & WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CA))
				git_error_set(GIT_ERROR_HTTP, "SSL certificate signed by unknown CA");
			else if ((status & WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CERT))
				git_error_set(GIT_ERROR_HTTP, "SSL certificate is invalid");
			else if ((status & WINHTTP_CALLBACK_STATUS_FLAG_CERT_REV_FAILED))
				git_error_set(GIT_ERROR_HTTP, "certificate revocation check failed");
			else if ((status & WINHTTP_CALLBACK_STATUS_FLAG_CERT_REVOKED))
				git_error_set(GIT_ERROR_HTTP, "SSL certificate was revoked");
			else if ((status & WINHTTP_CALLBACK_STATUS_FLAG_SECURITY_CHANNEL_ERROR))
				git_error_set(GIT_ERROR_HTTP, "security libraries could not be loaded");
			else
				git_error_set(GIT_ERROR_HTTP, "unknown security error %lu", status);

			break;

		case WINHTTP_CALLBACK_STATUS_SENDING_REQUEST:
			((winhttp_stream *) ctx)->status_sending_request_reached = 1;

			break;
	}
}

static int user_agent(bool *exists, git_str *out)
{
	const char *product = git_settings__user_agent_product();
	const char *comment = git_settings__user_agent();

	GIT_ASSERT(product && comment);

	if (!*product) {
		*exists = false;
		return 0;
	}

	git_str_puts(out, product);

	if (*comment) {
		git_str_puts(out, " (");
		git_str_puts(out, comment);
		git_str_puts(out, ")");
	}

	if (git_str_oom(out))
		return -1;

	*exists = true;
	return 0;
}

static int winhttp_connect(
	winhttp_subtransport *t)
{
	wchar_t *wide_host = NULL;
	int32_t port;
	wchar_t *wide_ua = NULL;
	git_str ipv6 = GIT_STR_INIT, ua = GIT_STR_INIT;
	const char *host;
	int error = -1;
	int default_timeout = TIMEOUT_INFINITE;
	int default_connect_timeout = DEFAULT_CONNECT_TIMEOUT;
	bool has_ua = true;
	DWORD protocols =
		WINHTTP_FLAG_SECURE_PROTOCOL_TLS1 |
		WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1 |
		WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2 |
		WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_3;

	t->session = NULL;
	t->connection = NULL;

	/* Prepare port */
	if (git__strntol32(&port, t->server.url.port,
			   strlen(t->server.url.port), NULL, 10) < 0)
		goto on_error;

	/* IPv6? Add braces around the host. */
	if (git_net_url_is_ipv6(&t->server.url)) {
		if (git_str_printf(&ipv6, "[%s]", t->server.url.host) < 0)
			goto on_error;

		host = ipv6.ptr;
	} else {
		host = t->server.url.host;
	}

	/* Prepare host */
	if (git_utf8_to_16_alloc(&wide_host, host) < 0) {
		git_error_set(GIT_ERROR_OS, "unable to convert host to wide characters");
		goto on_error;
	}

	if (user_agent(&has_ua, &ua) < 0)
		goto on_error;

	if (has_ua &&
	    git_utf8_to_16_alloc(&wide_ua, git_str_cstr(&ua)) < 0) {
		git_error_set(GIT_ERROR_OS, "unable to convert host to wide characters");
		goto on_error;
	}

	/* Establish session */
	t->session = WinHttpOpen(
		wide_ua,
		WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
		WINHTTP_NO_PROXY_NAME,
		WINHTTP_NO_PROXY_BYPASS,
		0);

	if (!t->session) {
		git_error_set(GIT_ERROR_OS, "failed to init WinHTTP");
		goto on_error;
	}

	/*
	 * Do a best-effort attempt to enable TLS 1.3 and 1.2 but allow this to
	 * fail; if TLS 1.2 or 1.3 support is not available for some reason,
	 * ignore the failure (it will keep the default protocols).
	 */
	if (WinHttpSetOption(t->session,
		WINHTTP_OPTION_SECURE_PROTOCOLS,
		&protocols,
		sizeof(protocols)) == FALSE) {
		protocols &= ~WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_3;
		WinHttpSetOption(t->session,
			WINHTTP_OPTION_SECURE_PROTOCOLS,
			&protocols,
			sizeof(protocols));
	}

	if (!WinHttpSetTimeouts(t->session, default_timeout, default_connect_timeout, default_timeout, default_timeout)) {
		git_error_set(GIT_ERROR_OS, "failed to set timeouts for WinHTTP");
		goto on_error;
	}


	/* Establish connection */
	t->connection = WinHttpConnect(
		t->session,
		wide_host,
		(INTERNET_PORT) port,
		0);

	if (!t->connection) {
		git_error_set(GIT_ERROR_OS, "failed to connect to host");
		goto on_error;
	}

	if (WinHttpSetStatusCallback(
			t->connection,
			winhttp_status,
			WINHTTP_CALLBACK_FLAG_SECURE_FAILURE | WINHTTP_CALLBACK_FLAG_SEND_REQUEST,
			0
		) == WINHTTP_INVALID_STATUS_CALLBACK) {
		git_error_set(GIT_ERROR_OS, "failed to set status callback");
		goto on_error;
	}

	error = 0;

on_error:
	if (error < 0)
		winhttp_close_connection(t);

	git_str_dispose(&ua);
	git_str_dispose(&ipv6);
	git__free(wide_host);
	git__free(wide_ua);

	return error;
}

static int do_send_request(winhttp_stream *s, size_t len, bool chunked)
{
	int attempts;
	bool success;

	if (len > DWORD_MAX) {
		SetLastError(ERROR_NOT_ENOUGH_MEMORY);
		return -1;
	}

	for (attempts = 0; attempts < 5; attempts++) {
		if (chunked) {
			success = WinHttpSendRequest(s->request,
				WINHTTP_NO_ADDITIONAL_HEADERS, 0,
				WINHTTP_NO_REQUEST_DATA, 0,
				WINHTTP_IGNORE_REQUEST_TOTAL_LENGTH, (DWORD_PTR)s);
		} else {
			success = WinHttpSendRequest(s->request,
				WINHTTP_NO_ADDITIONAL_HEADERS, 0,
				WINHTTP_NO_REQUEST_DATA, 0,
				(DWORD)len, (DWORD_PTR)s);
		}

		if (success || GetLastError() != (DWORD)SEC_E_BUFFER_TOO_SMALL)
			break;
	}

	return success ? 0 : -1;
}

static int send_request(winhttp_stream *s, size_t len, bool chunked)
{
	int request_failed = 1, error, attempts = 0;
	DWORD ignore_flags, send_request_error;

	git_error_clear();

	while (request_failed && attempts++ < 3) {
		int cert_valid = 1;
		int client_cert_requested = 0;
		request_failed = 0;
		if ((error = do_send_request(s, len, chunked)) < 0) {
			send_request_error = GetLastError();
			request_failed = 1;
			switch (send_request_error) {
				case ERROR_WINHTTP_SECURE_FAILURE:
					cert_valid = 0;
					break;
				case ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED:
					client_cert_requested = 1;
					break;
				default:
					git_error_set(GIT_ERROR_OS, "failed to send request");
					return -1;
			}
		}

		/*
		 * Only check the certificate if we were able to reach the sending request phase, or
		 * received a secure failure error. Otherwise, the server certificate won't be available
		 * since the request wasn't able to complete (e.g. proxy auth required)
		 */
		if (!cert_valid ||
			(!request_failed && s->status_sending_request_reached)) {
			git_error_clear();
			if ((error = certificate_check(s, cert_valid)) < 0) {
				if (git_error_last()->klass == GIT_ERROR_NONE)
					git_error_set(GIT_ERROR_OS, "user cancelled certificate check");

				return error;
			}
		}

		/* if neither the request nor the certificate check returned errors, we're done */
		if (!request_failed)
			return 0;

		if (!cert_valid) {
			ignore_flags = no_check_cert_flags;
			if (!WinHttpSetOption(s->request, WINHTTP_OPTION_SECURITY_FLAGS, &ignore_flags, sizeof(ignore_flags))) {
				git_error_set(GIT_ERROR_OS, "failed to set security options");
				return -1;
			}
		}

		if (client_cert_requested) {
			/*
			 * Client certificates are not supported, explicitly tell the server that
			 * (it's possible a client certificate was requested but is not required)
			 */
			if (!WinHttpSetOption(s->request, WINHTTP_OPTION_CLIENT_CERT_CONTEXT, WINHTTP_NO_CLIENT_CERT_CONTEXT, 0)) {
				git_error_set(GIT_ERROR_OS, "failed to set client cert context");
				return -1;
			}
		}
	}

	return error;
}

static int acquire_credentials(
	HINTERNET request,
	winhttp_server *server,
	const char *url_str,
	git_credential_acquire_cb cred_cb,
	void *cred_cb_payload)
{
	int allowed_types;
	int error = 1;

	if (parse_unauthorized_response(&allowed_types, &server->auth_mechanisms, request) < 0)
		return -1;

	if (allowed_types) {
		git_credential_free(server->cred);
		server->cred = NULL;

		/* Start with URL-specified credentials, if there were any. */
		if (!server->url_cred_presented && server->url.username && server->url.password) {
			error = acquire_url_cred(&server->cred, allowed_types, server->url.username, server->url.password);
			server->url_cred_presented = 1;

			if (error < 0)
				return error;
		}

		/* Next use the user-defined callback, if there is one. */
		if (error > 0 && cred_cb) {
			error = cred_cb(&server->cred, url_str, server->url.username, allowed_types, cred_cb_payload);

			/* Treat GIT_PASSTHROUGH as though git_credential_acquire_cb isn't set */
			if (error == GIT_PASSTHROUGH)
				error = 1;
			else if (error < 0)
				return error;
		}

		/* Finally, invoke the fallback default credential lookup. */
		if (error > 0) {
			error = acquire_fallback_cred(&server->cred, url_str, allowed_types);

			if (error < 0)
				return error;
		}
	}

	/*
	 * No error occurred but we could not find appropriate credentials.
	 * This behaves like a pass-through.
	 */
	return error;
}

static int winhttp_stream_read(
	git_smart_subtransport_stream *stream,
	char *buffer,
	size_t buf_size,
	size_t *bytes_read)
{
	winhttp_stream *s = (winhttp_stream *)stream;
	winhttp_subtransport *t = OWNING_SUBTRANSPORT(s);
	DWORD dw_bytes_read;
	char replay_count = 0;
	int error;

replay:
	/* Enforce a reasonable cap on the number of replays */
	if (replay_count++ >= GIT_HTTP_REPLAY_MAX) {
		git_error_set(GIT_ERROR_HTTP, "too many redirects or authentication replays");
		return GIT_ERROR; /* not GIT_EAUTH because the exact cause is not clear */
	}

	/* Connect if necessary */
	if (!s->request && winhttp_stream_connect(s) < 0)
		return -1;

	if (!s->received_response) {
		DWORD status_code, status_code_length, content_type_length, bytes_written;
		char expected_content_type_8[MAX_CONTENT_TYPE_LEN];
		wchar_t expected_content_type[MAX_CONTENT_TYPE_LEN], content_type[MAX_CONTENT_TYPE_LEN];

		if (!s->sent_request) {

			if ((error = send_request(s, s->post_body_len, false)) < 0)
				return error;

			s->sent_request = 1;
		}

		if (s->chunked) {
			GIT_ASSERT(s->verb == post_verb);

			/* Flush, if necessary */
			if (s->chunk_buffer_len > 0 &&
				write_chunk(s->request, s->chunk_buffer, s->chunk_buffer_len) < 0)
				return -1;

			s->chunk_buffer_len = 0;

			/* Write the final chunk. */
			if (!WinHttpWriteData(s->request,
				"0\r\n\r\n", 5,
				&bytes_written)) {
				git_error_set(GIT_ERROR_OS, "failed to write final chunk");
				return -1;
			}
		}
		else if (s->post_body) {
			char *buffer;
			DWORD len = s->post_body_len, bytes_read;

			if (INVALID_SET_FILE_POINTER == SetFilePointer(s->post_body,
					0, 0, FILE_BEGIN) &&
				NO_ERROR != GetLastError()) {
				git_error_set(GIT_ERROR_OS, "failed to reset file pointer");
				return -1;
			}

			buffer = git__malloc(CACHED_POST_BODY_BUF_SIZE);
			GIT_ERROR_CHECK_ALLOC(buffer);

			while (len > 0) {
				DWORD bytes_written;

				if (!ReadFile(s->post_body, buffer,
					min(CACHED_POST_BODY_BUF_SIZE, len),
					&bytes_read, NULL) ||
					!bytes_read) {
					git__free(buffer);
					git_error_set(GIT_ERROR_OS, "failed to read from temp file");
					return -1;
				}

				if (!WinHttpWriteData(s->request, buffer,
					bytes_read, &bytes_written)) {
					git__free(buffer);
					git_error_set(GIT_ERROR_OS, "failed to write data");
					return -1;
				}

				len -= bytes_read;
				GIT_ASSERT(bytes_read == bytes_written);
			}

			git__free(buffer);

			/* Eagerly close the temp file */
			CloseHandle(s->post_body);
			s->post_body = NULL;
		}

		if (!WinHttpReceiveResponse(s->request, 0)) {
			git_error_set(GIT_ERROR_OS, "failed to receive response");
			return -1;
		}

		/* Verify that we got a 200 back */
		status_code_length = sizeof(status_code);

		if (!WinHttpQueryHeaders(s->request,
			WINHTTP_QUERY_STATUS_CODE | WINHTTP_QUERY_FLAG_NUMBER,
			WINHTTP_HEADER_NAME_BY_INDEX,
			&status_code, &status_code_length,
			WINHTTP_NO_HEADER_INDEX)) {
				git_error_set(GIT_ERROR_OS, "failed to retrieve status code");
				return -1;
		}

		/* The implementation of WinHTTP prior to Windows 7 will not
		 * redirect to an identical URI. Some Git hosters use self-redirects
		 * as part of their DoS mitigation strategy. Check first to see if we
		 * have a redirect status code, and that we haven't already streamed
		 * a post body. (We can't replay a streamed POST.) */
		if (!s->chunked &&
			(HTTP_STATUS_MOVED == status_code ||
			 HTTP_STATUS_REDIRECT == status_code ||
			 (HTTP_STATUS_REDIRECT_METHOD == status_code &&
			  get_verb == s->verb) ||
			 HTTP_STATUS_REDIRECT_KEEP_VERB == status_code ||
			 HTTP_STATUS_PERMANENT_REDIRECT == status_code)) {

			/* Check for Windows 7. This workaround is only necessary on
			 * Windows Vista and earlier. Windows 7 is version 6.1. */
			wchar_t *location;
			DWORD location_length;
			char *location8;

			/* OK, fetch the Location header from the redirect. */
			if (WinHttpQueryHeaders(s->request,
				WINHTTP_QUERY_LOCATION,
				WINHTTP_HEADER_NAME_BY_INDEX,
				WINHTTP_NO_OUTPUT_BUFFER,
				&location_length,
				WINHTTP_NO_HEADER_INDEX) ||
				GetLastError() != ERROR_INSUFFICIENT_BUFFER) {
				git_error_set(GIT_ERROR_OS, "failed to read Location header");
				return -1;
			}

			location = git__malloc(location_length);
			GIT_ERROR_CHECK_ALLOC(location);

			if (!WinHttpQueryHeaders(s->request,
				WINHTTP_QUERY_LOCATION,
				WINHTTP_HEADER_NAME_BY_INDEX,
				location,
				&location_length,
				WINHTTP_NO_HEADER_INDEX)) {
				git_error_set(GIT_ERROR_OS, "failed to read Location header");
				git__free(location);
				return -1;
			}

			/* Convert the Location header to UTF-8 */
			if (git_utf8_from_16_alloc(&location8, location) < 0) {
				git_error_set(GIT_ERROR_OS, "failed to convert Location header to UTF-8");
				git__free(location);
				return -1;
			}

			git__free(location);

			/* Replay the request */
			winhttp_stream_close(s);

			if (!git__prefixcmp_icase(location8, prefix_https)) {
				bool follow = (t->owner->connect_opts.follow_redirects != GIT_REMOTE_REDIRECT_NONE);

				/* Upgrade to secure connection; disconnect and start over */
				if (git_net_url_apply_redirect(&t->server.url, location8, follow, s->service_url) < 0) {
					git__free(location8);
					return -1;
				}

				winhttp_close_connection(t);

				if (winhttp_connect(t) < 0)
					return -1;
			}

			git__free(location8);
			goto replay;
		}

		/* Handle authentication failures */
		if (status_code == HTTP_STATUS_DENIED) {
			int error = acquire_credentials(s->request,
				&t->server,
				t->owner->url,
				t->owner->connect_opts.callbacks.credentials,
				t->owner->connect_opts.callbacks.payload);

			if (error < 0) {
				return error;
			} else if (!error) {
				GIT_ASSERT(t->server.cred);
				winhttp_stream_close(s);
				goto replay;
			}
		} else if (status_code == HTTP_STATUS_PROXY_AUTH_REQ) {
			int error = acquire_credentials(s->request,
				&t->proxy,
				t->owner->connect_opts.proxy_opts.url,
				t->owner->connect_opts.proxy_opts.credentials,
				t->owner->connect_opts.proxy_opts.payload);

			if (error < 0) {
				return error;
			} else if (!error) {
				GIT_ASSERT(t->proxy.cred);
				winhttp_stream_close(s);
				goto replay;
			}
		}

		if (HTTP_STATUS_OK != status_code) {
			git_error_set(GIT_ERROR_HTTP, "request failed with status code: %lu", status_code);
			return -1;
		}

		/* Verify that we got the correct content-type back */
		if (post_verb == s->verb)
			p_snprintf(expected_content_type_8, MAX_CONTENT_TYPE_LEN, "application/x-git-%s-result", s->service);
		else
			p_snprintf(expected_content_type_8, MAX_CONTENT_TYPE_LEN, "application/x-git-%s-advertisement", s->service);

		if (git_utf8_to_16(expected_content_type, MAX_CONTENT_TYPE_LEN, expected_content_type_8) < 0) {
			git_error_set(GIT_ERROR_OS, "failed to convert expected content-type to wide characters");
			return -1;
		}

		content_type_length = sizeof(content_type);

		if (!WinHttpQueryHeaders(s->request,
			WINHTTP_QUERY_CONTENT_TYPE,
			WINHTTP_HEADER_NAME_BY_INDEX,
			&content_type, &content_type_length,
			WINHTTP_NO_HEADER_INDEX)) {
				git_error_set(GIT_ERROR_OS, "failed to retrieve response content-type");
				return -1;
		}

		if (wcscmp(expected_content_type, content_type)) {
			git_error_set(GIT_ERROR_HTTP, "received unexpected content-type");
			return -1;
		}

		s->received_response = 1;
	}

	if (!WinHttpReadData(s->request,
		(LPVOID)buffer,
		(DWORD)buf_size,
		&dw_bytes_read))
	{
		git_error_set(GIT_ERROR_OS, "failed to read data");
		return -1;
	}

	*bytes_read = dw_bytes_read;

	return 0;
}

static int winhttp_stream_write_single(
	git_smart_subtransport_stream *stream,
	const char *buffer,
	size_t len)
{
	winhttp_stream *s = (winhttp_stream *)stream;
	DWORD bytes_written;
	int error;

	if (!s->request && winhttp_stream_connect(s) < 0)
		return -1;

	/* This implementation of write permits only a single call. */
	if (s->sent_request) {
		git_error_set(GIT_ERROR_HTTP, "subtransport configured for only one write");
		return -1;
	}

	if ((error = send_request(s, len, false)) < 0)
		return error;

	s->sent_request = 1;

	if (!WinHttpWriteData(s->request,
			(LPCVOID)buffer,
			(DWORD)len,
			&bytes_written)) {
		git_error_set(GIT_ERROR_OS, "failed to write data");
		return -1;
	}

	GIT_ASSERT((DWORD)len == bytes_written);

	return 0;
}

static int put_uuid_string(LPWSTR buffer, size_t buffer_len_cch)
{
	UUID uuid;
	RPC_STATUS status = UuidCreate(&uuid);
	int result;

	if (RPC_S_OK != status &&
		RPC_S_UUID_LOCAL_ONLY != status &&
		RPC_S_UUID_NO_ADDRESS != status) {
		git_error_set(GIT_ERROR_HTTP, "unable to generate name for temp file");
		return -1;
	}

	if (buffer_len_cch < UUID_LENGTH_CCH + 1) {
		git_error_set(GIT_ERROR_HTTP, "buffer too small for name of temp file");
		return -1;
	}

#if !defined(__MINGW32__) || defined(MINGW_HAS_SECURE_API)
	result = swprintf_s(buffer, buffer_len_cch,
#else
	result = wsprintfW(buffer,
#endif
		L"%08x%04x%04x%02x%02x%02x%02x%02x%02x%02x%02x",
		uuid.Data1, uuid.Data2, uuid.Data3,
		uuid.Data4[0], uuid.Data4[1], uuid.Data4[2], uuid.Data4[3],
		uuid.Data4[4], uuid.Data4[5], uuid.Data4[6], uuid.Data4[7]);

	if (result < UUID_LENGTH_CCH) {
		git_error_set(GIT_ERROR_OS, "unable to generate name for temp file");
		return -1;
	}

	return 0;
}

static int get_temp_file(LPWSTR buffer, DWORD buffer_len_cch)
{
	size_t len;

	if (!GetTempPathW(buffer_len_cch, buffer)) {
		git_error_set(GIT_ERROR_OS, "failed to get temp path");
		return -1;
	}

	len = wcslen(buffer);

	if (buffer[len - 1] != '\\' && len < buffer_len_cch)
		buffer[len++] = '\\';

	if (put_uuid_string(&buffer[len], (size_t)buffer_len_cch - len) < 0)
		return -1;

	return 0;
}

static int winhttp_stream_write_buffered(
	git_smart_subtransport_stream *stream,
	const char *buffer,
	size_t len)
{
	winhttp_stream *s = (winhttp_stream *)stream;
	DWORD bytes_written;

	if (!s->request && winhttp_stream_connect(s) < 0)
		return -1;

	/* Buffer the payload, using a temporary file so we delegate
	 * memory management of the data to the operating system. */
	if (!s->post_body) {
		wchar_t temp_path[MAX_PATH + 1];

		if (get_temp_file(temp_path, MAX_PATH + 1) < 0)
			return -1;

		s->post_body = CreateFileW(temp_path,
			GENERIC_READ | GENERIC_WRITE,
			FILE_SHARE_DELETE, NULL,
			CREATE_NEW,
			FILE_ATTRIBUTE_TEMPORARY | FILE_FLAG_DELETE_ON_CLOSE | FILE_FLAG_SEQUENTIAL_SCAN,
			NULL);

		if (INVALID_HANDLE_VALUE == s->post_body) {
			s->post_body = NULL;
			git_error_set(GIT_ERROR_OS, "failed to create temporary file");
			return -1;
		}
	}

	if (!WriteFile(s->post_body, buffer, (DWORD)len, &bytes_written, NULL)) {
		git_error_set(GIT_ERROR_OS, "failed to write to temporary file");
		return -1;
	}

	GIT_ASSERT((DWORD)len == bytes_written);

	s->post_body_len += bytes_written;

	return 0;
}

static int winhttp_stream_write_chunked(
	git_smart_subtransport_stream *stream,
	const char *buffer,
	size_t len)
{
	winhttp_stream *s = (winhttp_stream *)stream;
	int error;

	if (!s->request && winhttp_stream_connect(s) < 0)
		return -1;

	if (!s->sent_request) {
		/* Send Transfer-Encoding: chunked header */
		if (!WinHttpAddRequestHeaders(s->request,
			transfer_encoding, (ULONG) -1L,
			WINHTTP_ADDREQ_FLAG_ADD)) {
			git_error_set(GIT_ERROR_OS, "failed to add a header to the request");
			return -1;
		}

		if ((error = send_request(s, 0, true)) < 0)
			return error;

		s->sent_request = 1;
	}

	if (len > CACHED_POST_BODY_BUF_SIZE) {
		/* Flush, if necessary */
		if (s->chunk_buffer_len > 0) {
			if (write_chunk(s->request, s->chunk_buffer, s->chunk_buffer_len) < 0)
				return -1;

			s->chunk_buffer_len = 0;
		}

		/* Write chunk directly */
		if (write_chunk(s->request, buffer, len) < 0)
			return -1;
	}
	else {
		/* Append as much to the buffer as we can */
		int count = (int)min(CACHED_POST_BODY_BUF_SIZE - s->chunk_buffer_len, len);

		if (!s->chunk_buffer) {
			s->chunk_buffer = git__malloc(CACHED_POST_BODY_BUF_SIZE);
			GIT_ERROR_CHECK_ALLOC(s->chunk_buffer);
		}

		memcpy(s->chunk_buffer + s->chunk_buffer_len, buffer, count);
		s->chunk_buffer_len += count;
		buffer += count;
		len -= count;

		/* Is the buffer full? If so, then flush */
		if (CACHED_POST_BODY_BUF_SIZE == s->chunk_buffer_len) {
			if (write_chunk(s->request, s->chunk_buffer, s->chunk_buffer_len) < 0)
				return -1;

			s->chunk_buffer_len = 0;

			/* Is there any remaining data from the source? */
			if (len > 0) {
				memcpy(s->chunk_buffer, buffer, len);
				s->chunk_buffer_len = (unsigned int)len;
			}
		}
	}

	return 0;
}

static void winhttp_stream_free(git_smart_subtransport_stream *stream)
{
	winhttp_stream *s = (winhttp_stream *)stream;

	winhttp_stream_close(s);
	git__free(s);
}

static int winhttp_stream_alloc(winhttp_subtransport *t, winhttp_stream **stream)
{
	winhttp_stream *s;

	if (!stream)
		return -1;

	s = git__calloc(1, sizeof(winhttp_stream));
	GIT_ERROR_CHECK_ALLOC(s);

	s->parent.subtransport = &t->parent;
	s->parent.read = winhttp_stream_read;
	s->parent.write = winhttp_stream_write_single;
	s->parent.free = winhttp_stream_free;

	*stream = s;

	return 0;
}

static int winhttp_uploadpack_ls(
	winhttp_subtransport *t,
	winhttp_stream *s)
{
	GIT_UNUSED(t);

	s->service = upload_pack_service;
	s->service_url = upload_pack_ls_service_url;
	s->verb = get_verb;

	return 0;
}

static int winhttp_uploadpack(
	winhttp_subtransport *t,
	winhttp_stream *s)
{
	GIT_UNUSED(t);

	s->service = upload_pack_service;
	s->service_url = upload_pack_service_url;
	s->verb = post_verb;

	return 0;
}

static int winhttp_receivepack_ls(
	winhttp_subtransport *t,
	winhttp_stream *s)
{
	GIT_UNUSED(t);

	s->service = receive_pack_service;
	s->service_url = receive_pack_ls_service_url;
	s->verb = get_verb;

	return 0;
}

static int winhttp_receivepack(
	winhttp_subtransport *t,
	winhttp_stream *s)
{
	GIT_UNUSED(t);

	/* WinHTTP only supports Transfer-Encoding: chunked
	 * on Windows Vista (NT 6.0) and higher. */
	s->chunked = git_has_win32_version(6, 0, 0);

	if (s->chunked)
		s->parent.write = winhttp_stream_write_chunked;
	else
		s->parent.write = winhttp_stream_write_buffered;

	s->service = receive_pack_service;
	s->service_url = receive_pack_service_url;
	s->verb = post_verb;

	return 0;
}

static int winhttp_action(
	git_smart_subtransport_stream **stream,
	git_smart_subtransport *subtransport,
	const char *url,
	git_smart_service_t action)
{
	winhttp_subtransport *t = (winhttp_subtransport *)subtransport;
	winhttp_stream *s;
	int ret = -1;

	if (!t->connection)
		if ((ret = git_net_url_parse(&t->server.url, url)) < 0 ||
			 (ret = winhttp_connect(t)) < 0)
			return ret;

	if (winhttp_stream_alloc(t, &s) < 0)
		return -1;

	if (!stream)
		return -1;

	switch (action)
	{
		case GIT_SERVICE_UPLOADPACK_LS:
			ret = winhttp_uploadpack_ls(t, s);
			break;

		case GIT_SERVICE_UPLOADPACK:
			ret = winhttp_uploadpack(t, s);
			break;

		case GIT_SERVICE_RECEIVEPACK_LS:
			ret = winhttp_receivepack_ls(t, s);
			break;

		case GIT_SERVICE_RECEIVEPACK:
			ret = winhttp_receivepack(t, s);
			break;

		default:
			GIT_ASSERT(0);
	}

	if (!ret)
		*stream = &s->parent;

	return ret;
}

static int winhttp_close(git_smart_subtransport *subtransport)
{
	winhttp_subtransport *t = (winhttp_subtransport *)subtransport;

	git_net_url_dispose(&t->server.url);
	git_net_url_dispose(&t->proxy.url);

	if (t->server.cred) {
		t->server.cred->free(t->server.cred);
		t->server.cred = NULL;
	}

	if (t->proxy.cred) {
		t->proxy.cred->free(t->proxy.cred);
		t->proxy.cred = NULL;
	}

	return winhttp_close_connection(t);
}

static void winhttp_free(git_smart_subtransport *subtransport)
{
	winhttp_subtransport *t = (winhttp_subtransport *)subtransport;

	winhttp_close(subtransport);

	git__free(t);
}

int git_smart_subtransport_http(git_smart_subtransport **out, git_transport *owner, void *param)
{
	winhttp_subtransport *t;

	GIT_UNUSED(param);

	if (!out)
		return -1;

	t = git__calloc(1, sizeof(winhttp_subtransport));
	GIT_ERROR_CHECK_ALLOC(t);

	t->owner = (transport_smart *)owner;
	t->parent.action = winhttp_action;
	t->parent.close = winhttp_close;
	t->parent.free = winhttp_free;

	*out = (git_smart_subtransport *) t;
	return 0;
}

#endif /* GIT_WINHTTP */
