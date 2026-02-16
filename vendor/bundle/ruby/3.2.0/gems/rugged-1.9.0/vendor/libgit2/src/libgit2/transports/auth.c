/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "auth.h"

#include "git2/sys/credential.h"

static int basic_next_token(
	git_str *out,
	git_http_auth_context *ctx,
	git_credential *c)
{
	git_credential_userpass_plaintext *cred;
	git_str raw = GIT_STR_INIT;
	int error = GIT_EAUTH;

	GIT_UNUSED(ctx);

	if (c->credtype != GIT_CREDENTIAL_USERPASS_PLAINTEXT) {
		git_error_set(GIT_ERROR_INVALID, "invalid credential type for basic auth");
		goto on_error;
	}

	cred = (git_credential_userpass_plaintext *)c;

	git_str_printf(&raw, "%s:%s", cred->username, cred->password);

	if (git_str_oom(&raw) ||
		git_str_puts(out, "Basic ") < 0 ||
		git_str_encode_base64(out, git_str_cstr(&raw), raw.size) < 0)
		goto on_error;

	error = 0;

on_error:
	if (raw.size)
		git__memzero(raw.ptr, raw.size);

	git_str_dispose(&raw);
	return error;
}

static git_http_auth_context basic_context = {
	GIT_HTTP_AUTH_BASIC,
	GIT_CREDENTIAL_USERPASS_PLAINTEXT,
	0,
	NULL,
	basic_next_token,
	NULL,
	NULL
};

int git_http_auth_basic(
	git_http_auth_context **out, const git_net_url *url)
{
	GIT_UNUSED(url);

	*out = &basic_context;
	return 0;
}

int git_http_auth_dummy(
	git_http_auth_context **out, const git_net_url *url)
{
	GIT_UNUSED(url);

	*out = NULL;
	return GIT_PASSTHROUGH;
}

