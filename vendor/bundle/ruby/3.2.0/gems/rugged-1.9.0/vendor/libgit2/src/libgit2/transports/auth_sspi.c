/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "auth_ntlm.h"
#include "auth_negotiate.h"

#ifdef GIT_WIN32

#define SECURITY_WIN32

#include "git2.h"
#include "auth.h"
#include "git2/sys/credential.h"

#include <windows.h>
#include <security.h>

typedef struct {
	git_http_auth_context parent;
	wchar_t *target;

	const char *package_name;
	size_t package_name_len;
	wchar_t *package_name_w;
	SecPkgInfoW *package_info;
	SEC_WINNT_AUTH_IDENTITY_W identity;
	CredHandle cred;
	CtxtHandle context;

	int has_identity : 1,
	    has_credentials : 1,
	    has_context : 1,
	    complete : 1;
	git_str challenge;
} http_auth_sspi_context;

static void sspi_reset_context(http_auth_sspi_context *ctx)
{
	if (ctx->has_identity) {
		git__free(ctx->identity.User);
		git__free(ctx->identity.Domain);
		git__free(ctx->identity.Password);

		memset(&ctx->identity, 0, sizeof(SEC_WINNT_AUTH_IDENTITY_W));

		ctx->has_identity = 0;
	}

	if (ctx->has_credentials) {
		FreeCredentialsHandle(&ctx->cred);
		memset(&ctx->cred, 0, sizeof(CredHandle));

		ctx->has_credentials = 0;
	}

	if (ctx->has_context) {
		DeleteSecurityContext(&ctx->context);
		memset(&ctx->context, 0, sizeof(CtxtHandle));

		ctx->has_context = 0;
	}

	ctx->complete = 0;

	git_str_dispose(&ctx->challenge);
}

static int sspi_set_challenge(
	git_http_auth_context *c,
	const char *challenge)
{
	http_auth_sspi_context *ctx = (http_auth_sspi_context *)c;
	size_t challenge_len = strlen(challenge);

	git_str_clear(&ctx->challenge);

	if (strncmp(challenge, ctx->package_name, ctx->package_name_len) != 0) {
		git_error_set(GIT_ERROR_NET, "invalid %s challenge from server", ctx->package_name);
		return -1;
	}

	/*
	 * A package type indicator without a base64 payload indicates the
	 * mechanism; it's not an actual challenge. Ignore it.
	 */
	if (challenge[ctx->package_name_len] == 0) {
		return 0;
	} else if (challenge[ctx->package_name_len] != ' ') {
		git_error_set(GIT_ERROR_NET, "invalid %s challenge from server", ctx->package_name);
		return -1;
	}

	if (git_str_decode_base64(&ctx->challenge,
	                          challenge + (ctx->package_name_len + 1),
	                          challenge_len - (ctx->package_name_len + 1)) < 0) {
		git_error_set(GIT_ERROR_NET, "invalid %s challenge from server", ctx->package_name);
		return -1;
	}

	GIT_ASSERT(ctx->challenge.size <= ULONG_MAX);
	return 0;
}

static int create_identity(
	SEC_WINNT_AUTH_IDENTITY_W **out,
	http_auth_sspi_context *ctx,
	git_credential *cred)
{
	git_credential_userpass_plaintext *userpass;
	wchar_t *username = NULL, *domain = NULL, *password = NULL;
	int username_len = 0, domain_len = 0, password_len = 0;
	const char *sep;

	if (cred->credtype == GIT_CREDENTIAL_DEFAULT) {
		*out = NULL;
		return 0;
	}

	if (cred->credtype != GIT_CREDENTIAL_USERPASS_PLAINTEXT) {
		git_error_set(GIT_ERROR_NET, "unknown credential type: %d", cred->credtype);
		return -1;
	}

	userpass = (git_credential_userpass_plaintext *)cred;

	if ((sep = strchr(userpass->username, '\\')) != NULL) {
		GIT_ASSERT(sep - userpass->username < INT_MAX);

		username_len = git_utf8_to_16_alloc(&username, sep + 1);
		domain_len = git_utf8_to_16_alloc_with_len(&domain,
			userpass->username, (int)(sep - userpass->username));
	} else {
		username_len = git_utf8_to_16_alloc(&username,
			userpass->username);
	}

	password_len = git_utf8_to_16_alloc(&password, userpass->password);

	if (username_len < 0 || domain_len < 0 || password_len < 0) {
		git__free(username);
		git__free(domain);
		git__free(password);
		return -1;
	}

	ctx->identity.Flags = SEC_WINNT_AUTH_IDENTITY_UNICODE;
	ctx->identity.User = username;
	ctx->identity.UserLength = (unsigned long)username_len;
	ctx->identity.Password = password;
	ctx->identity.PasswordLength = (unsigned long)password_len;
	ctx->identity.Domain = domain;
	ctx->identity.DomainLength = (unsigned long)domain_len;

	ctx->has_identity = 1;

	*out = &ctx->identity;

	return 0;
}

static int sspi_next_token(
	git_str *buf,
	git_http_auth_context *c,
	git_credential *cred)
{
	http_auth_sspi_context *ctx = (http_auth_sspi_context *)c;
	SEC_WINNT_AUTH_IDENTITY_W *identity = NULL;
	TimeStamp timestamp;
	DWORD context_flags;
	SecBuffer input_buf = { 0, SECBUFFER_TOKEN, NULL };
	SecBuffer output_buf = { 0, SECBUFFER_TOKEN, NULL };
	SecBufferDesc input_buf_desc = { SECBUFFER_VERSION, 1, &input_buf };
	SecBufferDesc output_buf_desc = { SECBUFFER_VERSION, 1, &output_buf };
	SECURITY_STATUS status;

	if (ctx->complete)
		sspi_reset_context(ctx);

	if (!ctx->has_context) {
		if (create_identity(&identity, ctx, cred) < 0)
			return -1;

		status = AcquireCredentialsHandleW(NULL, ctx->package_name_w,
				SECPKG_CRED_BOTH, NULL, identity, NULL,
				NULL, &ctx->cred, &timestamp);

		if (status != SEC_E_OK) {
			git_error_set(GIT_ERROR_OS, "could not acquire credentials");
			return -1;
		}

		ctx->has_credentials = 1;
	}

	context_flags = ISC_REQ_ALLOCATE_MEMORY |
	                ISC_REQ_CONFIDENTIALITY |
	                ISC_REQ_MUTUAL_AUTH;

	if (ctx->challenge.size > 0) {
		input_buf.BufferType = SECBUFFER_TOKEN;
		input_buf.cbBuffer = (unsigned long)ctx->challenge.size;
		input_buf.pvBuffer = ctx->challenge.ptr;
	}

	status = InitializeSecurityContextW(&ctx->cred,
		ctx->has_context ? &ctx->context : NULL,
		ctx->target,
		context_flags,
		0,
		SECURITY_NETWORK_DREP,
		ctx->has_context ? &input_buf_desc : NULL,
		0,
		ctx->has_context ? NULL : &ctx->context,
		&output_buf_desc,
		&context_flags,
		NULL);

	if (status == SEC_I_COMPLETE_AND_CONTINUE ||
	    status == SEC_I_COMPLETE_NEEDED)
		status = CompleteAuthToken(&ctx->context, &output_buf_desc);

	if (status == SEC_E_OK) {
		ctx->complete = 1;
	} else if (status != SEC_I_CONTINUE_NEEDED) {
		git_error_set(GIT_ERROR_OS, "could not initialize security context");
		return -1;
	}

	ctx->has_context = 1;
	git_str_clear(&ctx->challenge);

	if (output_buf.cbBuffer > 0) {
		git_str_put(buf, ctx->package_name, ctx->package_name_len);
		git_str_putc(buf, ' ');
		git_str_encode_base64(buf, output_buf.pvBuffer, output_buf.cbBuffer);

		FreeContextBuffer(output_buf.pvBuffer);

		if (git_str_oom(buf))
			return -1;
	}

	return 0;
}

static int sspi_is_complete(git_http_auth_context *c)
{
	http_auth_sspi_context *ctx = (http_auth_sspi_context *)c;

	return ctx->complete;
}

static void sspi_context_free(git_http_auth_context *c)
{
	http_auth_sspi_context *ctx = (http_auth_sspi_context *)c;

	sspi_reset_context(ctx);

	FreeContextBuffer(ctx->package_info);
	git__free(ctx->target);
	git__free(ctx);
}

static int sspi_init_context(
	git_http_auth_context **out,
	git_http_auth_t type,
	const git_net_url *url)
{
	http_auth_sspi_context *ctx;
	git_str target = GIT_STR_INIT;

	*out = NULL;

	ctx = git__calloc(1, sizeof(http_auth_sspi_context));
	GIT_ERROR_CHECK_ALLOC(ctx);

	switch (type) {
	case GIT_HTTP_AUTH_NTLM:
		ctx->package_name = "NTLM";
		ctx->package_name_len = CONST_STRLEN("NTLM");
		ctx->package_name_w = L"NTLM";
		ctx->parent.credtypes = GIT_CREDENTIAL_USERPASS_PLAINTEXT |
	                                GIT_CREDENTIAL_DEFAULT;
		break;
	case GIT_HTTP_AUTH_NEGOTIATE:
		ctx->package_name = "Negotiate";
		ctx->package_name_len = CONST_STRLEN("Negotiate");
		ctx->package_name_w = L"Negotiate";
		ctx->parent.credtypes = GIT_CREDENTIAL_DEFAULT;
		break;
	default:
		git_error_set(GIT_ERROR_NET, "unknown SSPI auth type: %d", ctx->parent.type);
		git__free(ctx);
		return -1;
	}

	if (QuerySecurityPackageInfoW(ctx->package_name_w, &ctx->package_info) != SEC_E_OK) {
		git_error_set(GIT_ERROR_OS, "could not query security package");
		git__free(ctx);
		return -1;
	}

	if (git_str_printf(&target, "http/%s", url->host) < 0 ||
	    git_utf8_to_16_alloc(&ctx->target, target.ptr) < 0) {
		FreeContextBuffer(ctx->package_info);
		git__free(ctx);
		return -1;
	}

	ctx->parent.type = type;
	ctx->parent.connection_affinity = 1;
	ctx->parent.set_challenge = sspi_set_challenge;
	ctx->parent.next_token = sspi_next_token;
	ctx->parent.is_complete = sspi_is_complete;
	ctx->parent.free = sspi_context_free;

	*out = (git_http_auth_context *)ctx;

	git_str_dispose(&target);
	return 0;
}

int git_http_auth_negotiate(
	git_http_auth_context **out,
	const git_net_url *url)
{
	return sspi_init_context(out, GIT_HTTP_AUTH_NEGOTIATE, url);
}

int git_http_auth_ntlm(
	git_http_auth_context **out,
	const git_net_url *url)
{
	return sspi_init_context(out, GIT_HTTP_AUTH_NTLM, url);
}

#endif /* GIT_WIN32 */
