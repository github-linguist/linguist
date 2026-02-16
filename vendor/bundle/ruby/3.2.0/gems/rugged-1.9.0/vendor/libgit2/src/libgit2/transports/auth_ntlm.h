/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_transports_auth_ntlm_h__
#define INCLUDE_transports_auth_ntlm_h__

#include "auth.h"

/* NTLM requires a full request/challenge/response */
#define GIT_AUTH_STEPS_NTLM 2

#if defined(GIT_NTLM) || defined(GIT_WIN32)

#if defined(GIT_OPENSSL)
# define CRYPT_OPENSSL
#elif defined(GIT_MBEDTLS)
# define CRYPT_MBEDTLS
#elif defined(GIT_SECURE_TRANSPORT)
# define CRYPT_COMMONCRYPTO
#endif

extern int git_http_auth_ntlm(
	git_http_auth_context **out,
	const git_net_url *url);

#else

#define git_http_auth_ntlm git_http_auth_dummy

#endif /* GIT_NTLM */

#endif

