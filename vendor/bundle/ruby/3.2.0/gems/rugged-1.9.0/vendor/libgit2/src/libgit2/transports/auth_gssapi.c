/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "auth_negotiate.h"

#if defined(GIT_GSSAPI) || defined(GIT_GSSFRAMEWORK)

#include "git2.h"
#include "auth.h"
#include "git2/sys/credential.h"

#ifdef GIT_GSSFRAMEWORK
#import <GSS/GSS.h>
#elif defined(GIT_GSSAPI)
#include <gssapi.h>
#include <krb5.h>
#endif

static gss_OID_desc gssapi_oid_spnego =
	{ 6, (void *) "\x2b\x06\x01\x05\x05\x02" };
static gss_OID_desc gssapi_oid_krb5 =
	{ 9, (void *) "\x2a\x86\x48\x86\xf7\x12\x01\x02\x02" };

static gss_OID gssapi_oids[] =
	{ &gssapi_oid_spnego, &gssapi_oid_krb5, NULL };

typedef struct {
	git_http_auth_context parent;
	unsigned configured : 1,
		complete : 1;
	git_str target;
	char *challenge;
	gss_ctx_id_t gss_context;
	gss_OID oid;
} http_auth_gssapi_context;

static void gssapi_err_set(
	OM_uint32 status_major,
	OM_uint32 status_minor,
	const char *message)
{
	gss_buffer_desc buffer = GSS_C_EMPTY_BUFFER;
	OM_uint32 status_display, context = 0;

	if (gss_display_status(&status_display, status_major, GSS_C_GSS_CODE,
		GSS_C_NO_OID, &context, &buffer) == GSS_S_COMPLETE) {
		git_error_set(GIT_ERROR_NET, "%s: %.*s (%d.%d)",
			message, (int)buffer.length, (const char *)buffer.value,
			status_major, status_minor);
		gss_release_buffer(&status_minor, &buffer);
	} else {
		git_error_set(GIT_ERROR_NET, "%s: unknown negotiate error (%d.%d)",
			message, status_major, status_minor);
	}
}

static int gssapi_set_challenge(
	git_http_auth_context *c,
	const char *challenge)
{
	http_auth_gssapi_context *ctx = (http_auth_gssapi_context *)c;

	GIT_ASSERT_ARG(ctx);
	GIT_ASSERT_ARG(challenge);
	GIT_ASSERT(ctx->configured);

	git__free(ctx->challenge);

	ctx->challenge = git__strdup(challenge);
	GIT_ERROR_CHECK_ALLOC(ctx->challenge);

	return 0;
}

static void gssapi_context_dispose(http_auth_gssapi_context *ctx)
{
	OM_uint32 status_minor;

	if (ctx->gss_context != GSS_C_NO_CONTEXT) {
		gss_delete_sec_context(
		    &status_minor, &ctx->gss_context, GSS_C_NO_BUFFER);
		ctx->gss_context = GSS_C_NO_CONTEXT;
	}

	git_str_dispose(&ctx->target);

	git__free(ctx->challenge);
	ctx->challenge = NULL;
}

static int gssapi_next_token(
	git_str *buf,
	git_http_auth_context *c,
	git_credential *cred)
{
	http_auth_gssapi_context *ctx = (http_auth_gssapi_context *)c;
	OM_uint32 status_major, status_minor;
	gss_buffer_desc target_buffer = GSS_C_EMPTY_BUFFER,
		input_token = GSS_C_EMPTY_BUFFER,
		output_token = GSS_C_EMPTY_BUFFER;
	gss_buffer_t input_token_ptr = GSS_C_NO_BUFFER;
	git_str input_buf = GIT_STR_INIT;
	gss_name_t server = NULL;
	gss_OID mech;
	size_t challenge_len;
	int error = 0;

	GIT_ASSERT_ARG(buf);
	GIT_ASSERT_ARG(ctx);
	GIT_ASSERT_ARG(cred);

	GIT_ASSERT(ctx->configured);
	GIT_ASSERT(cred->credtype == GIT_CREDENTIAL_DEFAULT);

	if (ctx->complete)
		return 0;

	target_buffer.value = (void *)ctx->target.ptr;
	target_buffer.length = ctx->target.size;

	status_major = gss_import_name(&status_minor, &target_buffer,
		GSS_C_NT_HOSTBASED_SERVICE, &server);

	if (GSS_ERROR(status_major)) {
		gssapi_err_set(status_major, status_minor,
			"could not parse principal");
		error = -1;
		goto done;
	}

	challenge_len = ctx->challenge ? strlen(ctx->challenge) : 0;

	if (challenge_len < 9 || memcmp(ctx->challenge, "Negotiate", 9) != 0) {
		git_error_set(GIT_ERROR_NET, "server did not request negotiate");
		error = -1;
		goto done;
	}

	if (challenge_len > 9) {
		if (git_str_decode_base64(&input_buf,
				ctx->challenge + 10, challenge_len - 10) < 0) {
			git_error_set(GIT_ERROR_NET, "invalid negotiate challenge from server");
			error = -1;
			goto done;
		}

		input_token.value = input_buf.ptr;
		input_token.length = input_buf.size;
		input_token_ptr = &input_token;
	} else if (ctx->gss_context != GSS_C_NO_CONTEXT) {
		gssapi_context_dispose(ctx);
	}

	mech = &gssapi_oid_spnego;

	status_major = gss_init_sec_context(
		&status_minor,
		GSS_C_NO_CREDENTIAL,
		&ctx->gss_context,
		server,
		mech,
		GSS_C_DELEG_FLAG | GSS_C_MUTUAL_FLAG,
		GSS_C_INDEFINITE,
		GSS_C_NO_CHANNEL_BINDINGS,
		input_token_ptr,
		NULL,
		&output_token,
		NULL,
		NULL);

	if (GSS_ERROR(status_major)) {
		gssapi_err_set(status_major, status_minor, "negotiate failure");
		error = -1;
		goto done;
	}

	/* This message merely told us auth was complete; we do not respond. */
	if (status_major == GSS_S_COMPLETE) {
		gssapi_context_dispose(ctx);
		ctx->complete = 1;
		goto done;
	}

	if (output_token.length == 0) {
		git_error_set(GIT_ERROR_NET, "GSSAPI did not return token");
		error = -1;
		goto done;
	}

	git_str_puts(buf, "Negotiate ");
	git_str_encode_base64(buf, output_token.value, output_token.length);

	if (git_str_oom(buf))
		error = -1;

done:
	gss_release_name(&status_minor, &server);
	gss_release_buffer(&status_minor, (gss_buffer_t) &output_token);
	git_str_dispose(&input_buf);
	return error;
}

static int gssapi_is_complete(git_http_auth_context *c)
{
	http_auth_gssapi_context *ctx = (http_auth_gssapi_context *)c;

	GIT_ASSERT_ARG(ctx);

	return (ctx->complete == 1);
}

static void gssapi_context_free(git_http_auth_context *c)
{
	http_auth_gssapi_context *ctx = (http_auth_gssapi_context *)c;

	gssapi_context_dispose(ctx);

	ctx->configured = 0;
	ctx->complete = 0;
	ctx->oid = NULL;

	git__free(ctx);
}

static int gssapi_init_context(
	http_auth_gssapi_context *ctx,
	const git_net_url *url)
{
	OM_uint32 status_major, status_minor;
	gss_OID item, *oid;
	gss_OID_set mechanism_list;
	size_t i;

	/* Query supported mechanisms looking for SPNEGO) */
	status_major = gss_indicate_mechs(&status_minor, &mechanism_list);

	if (GSS_ERROR(status_major)) {
		gssapi_err_set(status_major, status_minor,
			"could not query mechanisms");
		return -1;
	}

	if (mechanism_list) {
		for (oid = gssapi_oids; *oid; oid++) {
			for (i = 0; i < mechanism_list->count; i++) {
				item = &mechanism_list->elements[i];

				if (item->length == (*oid)->length &&
					memcmp(item->elements, (*oid)->elements, item->length) == 0) {
					ctx->oid = *oid;
					break;
				}

			}

			if (ctx->oid)
				break;
		}
	}

	gss_release_oid_set(&status_minor, &mechanism_list);

	if (!ctx->oid) {
		git_error_set(GIT_ERROR_NET, "negotiate authentication is not supported");
		return GIT_EAUTH;
	}

	git_str_puts(&ctx->target, "HTTP@");
	git_str_puts(&ctx->target, url->host);

	if (git_str_oom(&ctx->target))
		return -1;

	ctx->gss_context = GSS_C_NO_CONTEXT;
	ctx->configured = 1;

	return 0;
}

int git_http_auth_negotiate(
	git_http_auth_context **out,
	const git_net_url *url)
{
	http_auth_gssapi_context *ctx;

	*out = NULL;

	ctx = git__calloc(1, sizeof(http_auth_gssapi_context));
	GIT_ERROR_CHECK_ALLOC(ctx);

	if (gssapi_init_context(ctx, url) < 0) {
		git__free(ctx);
		return -1;
	}

	ctx->parent.type = GIT_HTTP_AUTH_NEGOTIATE;
	ctx->parent.credtypes = GIT_CREDENTIAL_DEFAULT;
	ctx->parent.connection_affinity = 1;
	ctx->parent.set_challenge = gssapi_set_challenge;
	ctx->parent.next_token = gssapi_next_token;
	ctx->parent.is_complete = gssapi_is_complete;
	ctx->parent.free = gssapi_context_free;

	*out = (git_http_auth_context *)ctx;

	return 0;
}

#endif /* GIT_GSSAPI */

