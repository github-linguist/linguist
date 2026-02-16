/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_transports_auth_h__
#define INCLUDE_transports_auth_h__

#include "common.h"
#include "net.h"

typedef enum {
	GIT_HTTP_AUTH_BASIC = 1,
	GIT_HTTP_AUTH_NEGOTIATE = 2,
	GIT_HTTP_AUTH_NTLM = 4
} git_http_auth_t;

typedef struct git_http_auth_context git_http_auth_context;

struct git_http_auth_context {
	/** Type of scheme */
	git_http_auth_t type;

	/** Supported credentials */
	git_credential_t credtypes;

	/** Connection affinity or request affinity */
	unsigned connection_affinity : 1;

	/** Sets the challenge on the authentication context */
	int (*set_challenge)(git_http_auth_context *ctx, const char *challenge);

	/** Gets the next authentication token from the context */
	int (*next_token)(git_str *out, git_http_auth_context *ctx, git_credential *cred);

	/** Examines if all tokens have been presented. */
	int (*is_complete)(git_http_auth_context *ctx);

	/** Frees the authentication context */
	void (*free)(git_http_auth_context *ctx);
};

typedef struct {
	/** Type of scheme */
	git_http_auth_t type;

	/** Name of the scheme (as used in the Authorization header) */
	const char *name;

	/** Credential types this scheme supports */
	git_credential_t credtypes;

	/** Function to initialize an authentication context */
	int (*init_context)(
		git_http_auth_context **out,
		const git_net_url *url);
} git_http_auth_scheme;

int git_http_auth_dummy(
	git_http_auth_context **out,
	const git_net_url *url);

int git_http_auth_basic(
	git_http_auth_context **out,
	const git_net_url *url);

#endif
