/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2/errors.h"

#include "common.h"
#include "streams/registry.h"
#include "streams/tls.h"
#include "streams/mbedtls.h"
#include "streams/openssl.h"
#include "streams/stransport.h"
#include "streams/schannel.h"

int git_tls_stream_new(git_stream **out, const char *host, const char *port)
{
	int (*init)(git_stream **, const char *, const char *) = NULL;
	git_stream_registration custom = {0};
	int error;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(host);
	GIT_ASSERT_ARG(port);

	if ((error = git_stream_registry_lookup(&custom, GIT_STREAM_TLS)) == 0) {
		init = custom.init;
	} else if (error == GIT_ENOTFOUND) {
#ifdef GIT_SECURE_TRANSPORT
		init = git_stransport_stream_new;
#elif defined(GIT_OPENSSL)
		init = git_openssl_stream_new;
#elif defined(GIT_MBEDTLS)
		init = git_mbedtls_stream_new;
#elif defined(GIT_SCHANNEL)
		init = git_schannel_stream_new;
#endif
	} else {
		return error;
	}

	if (!init) {
		git_error_set(GIT_ERROR_SSL, "there is no TLS stream available");
		return -1;
	}

	return init(out, host, port);
}

int git_tls_stream_wrap(git_stream **out, git_stream *in, const char *host)
{
	int (*wrap)(git_stream **, git_stream *, const char *) = NULL;
	git_stream_registration custom = {0};

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(in);

	if (git_stream_registry_lookup(&custom, GIT_STREAM_TLS) == 0) {
		wrap = custom.wrap;
	} else {
#ifdef GIT_SECURE_TRANSPORT
		wrap = git_stransport_stream_wrap;
#elif defined(GIT_OPENSSL)
		wrap = git_openssl_stream_wrap;
#elif defined(GIT_MBEDTLS)
		wrap = git_mbedtls_stream_wrap;
#elif defined(GIT_SCHANNEL)
		wrap = git_schannel_stream_wrap;
#endif
	}

	if (!wrap) {
		git_error_set(GIT_ERROR_SSL, "there is no TLS stream available");
		return -1;
	}

	return wrap(out, in, host);
}
