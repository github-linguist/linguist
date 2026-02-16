/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include <git2.h>
#include "alloc.h"
#include "buf.h"
#include "common.h"
#include "filter.h"
#include "hash.h"
#include "merge_driver.h"
#include "pool.h"
#include "mwindow.h"
#include "oid.h"
#include "rand.h"
#include "runtime.h"
#include "settings.h"
#include "sysdir.h"
#include "thread.h"
#include "git2/global.h"
#include "streams/registry.h"
#include "streams/mbedtls.h"
#include "streams/openssl.h"
#include "streams/socket.h"
#include "transports/ssh_libssh2.h"

#ifdef GIT_WIN32
# include "win32/w32_leakcheck.h"
#endif

int git_libgit2_init(void)
{
	static git_runtime_init_fn init_fns[] = {
#ifdef GIT_WIN32
		git_win32_leakcheck_global_init,
#endif
		git_allocator_global_init,
		git_error_global_init,
		git_threads_global_init,
		git_oid_global_init,
		git_rand_global_init,
		git_hash_global_init,
		git_sysdir_global_init,
		git_filter_global_init,
		git_merge_driver_global_init,
		git_transport_ssh_libssh2_global_init,
		git_stream_registry_global_init,
		git_socket_stream_global_init,
		git_openssl_stream_global_init,
		git_mbedtls_stream_global_init,
		git_mwindow_global_init,
		git_pool_global_init,
		git_settings_global_init
	};

	return git_runtime_init(init_fns, ARRAY_SIZE(init_fns));
}

int git_libgit2_shutdown(void)
{
	return git_runtime_shutdown();
}

int git_libgit2_version(int *major, int *minor, int *rev)
{
	*major = LIBGIT2_VERSION_MAJOR;
	*minor = LIBGIT2_VERSION_MINOR;
	*rev = LIBGIT2_VERSION_REVISION;

	return 0;
}

const char *git_libgit2_prerelease(void)
{
	return LIBGIT2_VERSION_PRERELEASE;
}

int git_libgit2_features(void)
{
	return 0
#ifdef GIT_THREADS
		| GIT_FEATURE_THREADS
#endif
#ifdef GIT_HTTPS
		| GIT_FEATURE_HTTPS
#endif
#ifdef GIT_SSH
		| GIT_FEATURE_SSH
#endif
#ifdef GIT_USE_NSEC
		| GIT_FEATURE_NSEC
#endif
		| GIT_FEATURE_HTTP_PARSER
		| GIT_FEATURE_REGEX
#ifdef GIT_USE_ICONV
		| GIT_FEATURE_I18N
#endif
#if defined(GIT_NTLM) || defined(GIT_WIN32)
		| GIT_FEATURE_AUTH_NTLM
#endif
#if defined(GIT_GSSAPI) || defined(GIT_GSSFRAMEWORK) || defined(GIT_WIN32)
		| GIT_FEATURE_AUTH_NEGOTIATE
#endif
		| GIT_FEATURE_COMPRESSION
		| GIT_FEATURE_SHA1
#ifdef GIT_EXPERIMENTAL_SHA256
		| GIT_FEATURE_SHA256
#endif
	;
}

const char *git_libgit2_feature_backend(git_feature_t feature)
{
	switch (feature) {
	case GIT_FEATURE_THREADS:
#if defined(GIT_THREADS) && defined(GIT_WIN32)
		return "win32";
#elif defined(GIT_THREADS)
		return "pthread";
#endif
		break;

	case GIT_FEATURE_HTTPS:
#if defined(GIT_HTTPS) && defined(GIT_OPENSSL)
		return "openssl";
#elif defined(GIT_HTTPS) && defined(GIT_OPENSSL_DYNAMIC)
		return "openssl-dynamic";
#elif defined(GIT_HTTPS) && defined(GIT_MBEDTLS)
		return "mbedtls";
#elif defined(GIT_HTTPS) && defined(GIT_SECURE_TRANSPORT)
		return "securetransport";
#elif defined(GIT_HTTPS) && defined(GIT_SCHANNEL)
		return "schannel";
#elif defined(GIT_HTTPS) && defined(GIT_WINHTTP)
		return "winhttp";
#elif defined(GIT_HTTPS)
		GIT_ASSERT_WITH_RETVAL(!"Unknown HTTPS backend", NULL);
#endif
		break;

	case GIT_FEATURE_SSH:
#if defined(GIT_SSH_EXEC)
		return "exec";
#elif defined(GIT_SSH_LIBSSH2)
		return "libssh2";
#elif defined(GIT_SSH)
		GIT_ASSERT_WITH_RETVAL(!"Unknown SSH backend", NULL);
#endif
		break;

	case GIT_FEATURE_NSEC:
#if defined(GIT_USE_NSEC) && defined(GIT_USE_STAT_MTIMESPEC)
		return "mtimespec";
#elif defined(GIT_USE_NSEC) && defined(GIT_USE_STAT_MTIM)
		return "mtim";
#elif defined(GIT_USE_NSEC) && defined(GIT_USE_STAT_MTIME_NSEC)
		return "mtime";
#elif defined(GIT_USE_NSEC) && defined(GIT_WIN32)
		return "win32";
#elif defined(GIT_USE_NSEC)
		GIT_ASSERT_WITH_RETVAL(!"Unknown high-resolution time backend", NULL);
#endif
		break;

	case GIT_FEATURE_HTTP_PARSER:
#if defined(GIT_HTTPPARSER_HTTPPARSER)
		return "httpparser";
#elif defined(GIT_HTTPPARSER_LLHTTP)
		return "llhttp";
#elif defined(GIT_HTTPPARSER_BUILTIN)
		return "builtin";
#endif
		GIT_ASSERT_WITH_RETVAL(!"Unknown HTTP parser backend", NULL);
		break;

	case GIT_FEATURE_REGEX:
#if defined(GIT_REGEX_REGCOMP_L)
		return "regcomp_l";
#elif defined(GIT_REGEX_REGCOMP)
		return "regcomp";
#elif defined(GIT_REGEX_PCRE)
		return "pcre";
#elif defined(GIT_REGEX_PCRE2)
		return "pcre2";
#elif defined(GIT_REGEX_BUILTIN)
		return "builtin";
#endif
		GIT_ASSERT_WITH_RETVAL(!"Unknown regular expression backend", NULL);
		break;

	case GIT_FEATURE_I18N:
#if defined(GIT_USE_ICONV)
		return "iconv";
#endif
		break;

	case GIT_FEATURE_AUTH_NTLM:
#if defined(GIT_NTLM)
		return "ntlmclient";
#elif defined(GIT_WIN32)
		return "sspi";
#endif
		break;

	case GIT_FEATURE_AUTH_NEGOTIATE:
#if defined(GIT_GSSAPI)
		return "gssapi";
#elif defined(GIT_WIN32)
		return "sspi";
#endif
		break;

	case GIT_FEATURE_COMPRESSION:
#if defined(GIT_COMPRESSION_ZLIB)
		return "zlib";
#elif defined(GIT_COMPRESSION_BUILTIN)
		return "builtin";
#else
		GIT_ASSERT_WITH_RETVAL(!"Unknown compression backend", NULL);
#endif
		break;

	case GIT_FEATURE_SHA1:
#if defined(GIT_SHA1_COLLISIONDETECT)
		return "builtin";
#elif defined(GIT_SHA1_OPENSSL)
		return "openssl";
#elif defined(GIT_SHA1_OPENSSL_FIPS)
		return "openssl-fips";
#elif defined(GIT_SHA1_OPENSSL_DYNAMIC)
		return "openssl-dynamic";
#elif defined(GIT_SHA1_MBEDTLS)
		return "mbedtls";
#elif defined(GIT_SHA1_COMMON_CRYPTO)
		return "commoncrypto";
#elif defined(GIT_SHA1_WIN32)
		return "win32";
#else
		GIT_ASSERT_WITH_RETVAL(!"Unknown SHA1 backend", NULL);
#endif
		break;

	case GIT_FEATURE_SHA256:
#if defined(GIT_EXPERIMENTAL_SHA256) && defined(GIT_SHA256_BUILTIN)
		return "builtin";
#elif defined(GIT_EXPERIMENTAL_SHA256) && defined(GIT_SHA256_OPENSSL)
		return "openssl";
#elif defined(GIT_EXPERIMENTAL_SHA256) && defined(GIT_SHA256_OPENSSL_FIPS)
		return "openssl-fips";
#elif defined(GIT_EXPERIMENTAL_SHA256) && defined(GIT_SHA256_OPENSSL_DYNAMIC)
		return "openssl-dynamic";
#elif defined(GIT_EXPERIMENTAL_SHA256) && defined(GIT_SHA256_MBEDTLS)
		return "mbedtls";
#elif defined(GIT_EXPERIMENTAL_SHA256) && defined(GIT_SHA256_COMMON_CRYPTO)
		return "commoncrypto";
#elif defined(GIT_EXPERIMENTAL_SHA256) && defined(GIT_SHA256_WIN32)
		return "win32";
#elif defined(GIT_EXPERIMENTAL_SHA256)
		GIT_ASSERT_WITH_RETVAL(!"Unknown SHA256 backend", NULL);
#endif
		break;
	}

	return NULL;
}
