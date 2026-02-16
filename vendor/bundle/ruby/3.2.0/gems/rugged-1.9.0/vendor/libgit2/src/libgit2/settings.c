/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "settings.h"

#include <git2.h>
#include "alloc.h"
#include "buf.h"
#include "cache.h"
#include "common.h"
#include "filter.h"
#include "grafts.h"
#include "hash.h"
#include "index.h"
#include "merge_driver.h"
#include "pool.h"
#include "mwindow.h"
#include "object.h"
#include "odb.h"
#include "rand.h"
#include "refs.h"
#include "runtime.h"
#include "sysdir.h"
#include "thread.h"
#include "git2/global.h"
#include "streams/registry.h"
#include "streams/mbedtls.h"
#include "streams/openssl.h"
#include "streams/socket.h"
#include "transports/smart.h"
#include "transports/http.h"
#include "transports/ssh_libssh2.h"

#ifdef GIT_WIN32
# include "win32/w32_leakcheck.h"
#endif

/* Declarations for tuneable settings */
extern size_t git_mwindow__window_size;
extern size_t git_mwindow__mapped_limit;
extern size_t git_mwindow__file_limit;
extern size_t git_indexer__max_objects;
extern bool git_disable_pack_keep_file_checks;
extern int git_odb__packed_priority;
extern int git_odb__loose_priority;
extern int git_socket_stream__connect_timeout;
extern int git_socket_stream__timeout;

char *git__user_agent;
char *git__user_agent_product;
char *git__ssl_ciphers;

static void settings_global_shutdown(void)
{
	git__free(git__user_agent);
	git__free(git__user_agent_product);

	git__free(git__ssl_ciphers);
	git_repository__free_extensions();
}

int git_settings_global_init(void)
{
	return git_runtime_shutdown_register(settings_global_shutdown);
}

static int config_level_to_sysdir(int *out, int config_level)
{
	switch (config_level) {
	case GIT_CONFIG_LEVEL_SYSTEM:
		*out = GIT_SYSDIR_SYSTEM;
		return 0;
	case GIT_CONFIG_LEVEL_XDG:
		*out = GIT_SYSDIR_XDG;
		return 0;
	case GIT_CONFIG_LEVEL_GLOBAL:
		*out = GIT_SYSDIR_GLOBAL;
		return 0;
	case GIT_CONFIG_LEVEL_PROGRAMDATA:
		*out = GIT_SYSDIR_PROGRAMDATA;
		return 0;
	default:
		break;
	}

	git_error_set(
		GIT_ERROR_INVALID, "invalid config path selector %d", config_level);
	return -1;
}

const char *git_settings__user_agent_product(void)
{
	return git__user_agent_product ? git__user_agent_product :
		"git/2.0";
}

const char *git_settings__user_agent(void)
{
	return git__user_agent ? git__user_agent :
		"libgit2 " LIBGIT2_VERSION;
}

int git_libgit2_opts(int key, ...)
{
	int error = 0;
	va_list ap;

	va_start(ap, key);

	switch (key) {
	case GIT_OPT_SET_MWINDOW_SIZE:
		git_mwindow__window_size = va_arg(ap, size_t);
		break;

	case GIT_OPT_GET_MWINDOW_SIZE:
		*(va_arg(ap, size_t *)) = git_mwindow__window_size;
		break;

	case GIT_OPT_SET_MWINDOW_MAPPED_LIMIT:
		git_mwindow__mapped_limit = va_arg(ap, size_t);
		break;

	case GIT_OPT_GET_MWINDOW_MAPPED_LIMIT:
		*(va_arg(ap, size_t *)) = git_mwindow__mapped_limit;
		break;

	case GIT_OPT_SET_MWINDOW_FILE_LIMIT:
		git_mwindow__file_limit = va_arg(ap, size_t);
		break;

	case GIT_OPT_GET_MWINDOW_FILE_LIMIT:
		*(va_arg(ap, size_t *)) = git_mwindow__file_limit;
		break;

	case GIT_OPT_GET_SEARCH_PATH:
		{
			int sysdir = va_arg(ap, int);
			git_buf *out = va_arg(ap, git_buf *);
			git_str str = GIT_STR_INIT;
			const git_str *tmp;
			int level;

			if ((error = git_buf_tostr(&str, out)) < 0 ||
			    (error = config_level_to_sysdir(&level, sysdir)) < 0 ||
			    (error = git_sysdir_get(&tmp, level)) < 0 ||
			    (error = git_str_put(&str, tmp->ptr, tmp->size)) < 0)
				break;

			error = git_buf_fromstr(out, &str);
		}
		break;

	case GIT_OPT_SET_SEARCH_PATH:
		{
			int level;

			if ((error = config_level_to_sysdir(&level, va_arg(ap, int))) >= 0)
				error = git_sysdir_set(level, va_arg(ap, const char *));
		}
		break;

	case GIT_OPT_SET_CACHE_OBJECT_LIMIT:
		{
			git_object_t type = (git_object_t)va_arg(ap, int);
			size_t size = va_arg(ap, size_t);
			error = git_cache_set_max_object_size(type, size);
			break;
		}

	case GIT_OPT_SET_CACHE_MAX_SIZE:
		git_cache__max_storage = va_arg(ap, ssize_t);
		break;

	case GIT_OPT_ENABLE_CACHING:
		git_cache__enabled = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_GET_CACHED_MEMORY:
		*(va_arg(ap, ssize_t *)) = git_cache__current_storage.val;
		*(va_arg(ap, ssize_t *)) = git_cache__max_storage;
		break;

	case GIT_OPT_GET_TEMPLATE_PATH:
		{
			git_buf *out = va_arg(ap, git_buf *);
			git_str str = GIT_STR_INIT;
			const git_str *tmp;

			if ((error = git_buf_tostr(&str, out)) < 0 ||
			    (error = git_sysdir_get(&tmp, GIT_SYSDIR_TEMPLATE)) < 0 ||
			    (error = git_str_put(&str, tmp->ptr, tmp->size)) < 0)
				break;

			error = git_buf_fromstr(out, &str);
		}
		break;

	case GIT_OPT_SET_TEMPLATE_PATH:
		error = git_sysdir_set(GIT_SYSDIR_TEMPLATE, va_arg(ap, const char *));
		break;

	case GIT_OPT_SET_SSL_CERT_LOCATIONS:
#ifdef GIT_OPENSSL
		{
			const char *file = va_arg(ap, const char *);
			const char *path = va_arg(ap, const char *);
			error = git_openssl__set_cert_location(file, path);
		}
#elif defined(GIT_MBEDTLS)
		{
			const char *file = va_arg(ap, const char *);
			const char *path = va_arg(ap, const char *);
			error = git_mbedtls__set_cert_location(file, path);
		}
#else
		git_error_set(GIT_ERROR_SSL, "TLS backend doesn't support certificate locations");
		error = -1;
#endif
		break;

	case GIT_OPT_ADD_SSL_X509_CERT:
#ifdef GIT_OPENSSL
		{
			X509 *cert = va_arg(ap, X509 *);
			error = git_openssl__add_x509_cert(cert);
		}
#else
		git_error_set(GIT_ERROR_SSL, "TLS backend doesn't support adding of the raw certs");
		error = -1;
#endif
		break;

	case GIT_OPT_SET_USER_AGENT:
		{
			const char *new_agent = va_arg(ap, const char *);

			git__free(git__user_agent);

			if (new_agent) {
				git__user_agent= git__strdup(new_agent);

				if (!git__user_agent)
					error = -1;
			} else {
				git__user_agent = NULL;
			}
		}
		break;

	case GIT_OPT_GET_USER_AGENT:
		{
			git_buf *out = va_arg(ap, git_buf *);
			git_str str = GIT_STR_INIT;

			if ((error = git_buf_tostr(&str, out)) < 0 ||
			    (error = git_str_puts(&str, git_settings__user_agent())) < 0)
				break;

			error = git_buf_fromstr(out, &str);
		}
		break;

	case GIT_OPT_SET_USER_AGENT_PRODUCT:
		{
			const char *new_agent = va_arg(ap, const char *);

			git__free(git__user_agent_product);

			if (new_agent) {
				git__user_agent_product = git__strdup(new_agent);

				if (!git__user_agent_product)
					error = -1;
			} else {
				git__user_agent_product = NULL;
			}
		}
		break;

	case GIT_OPT_GET_USER_AGENT_PRODUCT:
		{
			git_buf *out = va_arg(ap, git_buf *);
			git_str str = GIT_STR_INIT;

			if ((error = git_buf_tostr(&str, out)) < 0 ||
			    (error = git_str_puts(&str, git_settings__user_agent_product())) < 0)
				break;

			error = git_buf_fromstr(out, &str);
		}
		break;

	case GIT_OPT_ENABLE_STRICT_OBJECT_CREATION:
		git_object__strict_input_validation = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_ENABLE_STRICT_SYMBOLIC_REF_CREATION:
		git_reference__enable_symbolic_ref_target_validation = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_SET_SSL_CIPHERS:
#if (GIT_OPENSSL || GIT_MBEDTLS)
		{
			git__free(git__ssl_ciphers);
			git__ssl_ciphers = git__strdup(va_arg(ap, const char *));
			if (!git__ssl_ciphers) {
				git_error_set_oom();
				error = -1;
			}
		}
#else
		git_error_set(GIT_ERROR_SSL, "TLS backend doesn't support custom ciphers");
		error = -1;
#endif
		break;

	case GIT_OPT_ENABLE_OFS_DELTA:
		git_smart__ofs_delta_enabled = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_ENABLE_FSYNC_GITDIR:
		git_repository__fsync_gitdir = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_GET_WINDOWS_SHAREMODE:
#ifdef GIT_WIN32
		*(va_arg(ap, unsigned long *)) = git_win32__createfile_sharemode;
#endif
		break;

	case GIT_OPT_SET_WINDOWS_SHAREMODE:
#ifdef GIT_WIN32
		git_win32__createfile_sharemode = va_arg(ap, unsigned long);
#endif
		break;

	case GIT_OPT_ENABLE_STRICT_HASH_VERIFICATION:
		git_odb__strict_hash_verification = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_SET_ALLOCATOR:
		error = git_allocator_setup(va_arg(ap, git_allocator *));
		break;

	case GIT_OPT_ENABLE_UNSAVED_INDEX_SAFETY:
		git_index__enforce_unsaved_safety = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_SET_PACK_MAX_OBJECTS:
		git_indexer__max_objects = va_arg(ap, size_t);
		break;

	case GIT_OPT_GET_PACK_MAX_OBJECTS:
		*(va_arg(ap, size_t *)) = git_indexer__max_objects;
		break;

	case GIT_OPT_DISABLE_PACK_KEEP_FILE_CHECKS:
		git_disable_pack_keep_file_checks = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_ENABLE_HTTP_EXPECT_CONTINUE:
		git_http__expect_continue = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_SET_ODB_PACKED_PRIORITY:
		git_odb__packed_priority = va_arg(ap, int);
		break;

	case GIT_OPT_SET_ODB_LOOSE_PRIORITY:
		git_odb__loose_priority = va_arg(ap, int);
		break;

	case GIT_OPT_SET_EXTENSIONS:
		{
			const char **extensions = va_arg(ap, const char **);
			size_t len = va_arg(ap, size_t);
			error = git_repository__set_extensions(extensions, len);
		}
		break;

	case GIT_OPT_GET_EXTENSIONS:
		{
			git_strarray *out = va_arg(ap, git_strarray *);
			char **extensions;
			size_t len;

			if ((error = git_repository__extensions(&extensions, &len)) < 0)
				break;

			out->strings = extensions;
			out->count = len;
		}
		break;

	case GIT_OPT_GET_OWNER_VALIDATION:
		*(va_arg(ap, int *)) = git_repository__validate_ownership;
		break;

	case GIT_OPT_SET_OWNER_VALIDATION:
		git_repository__validate_ownership = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_GET_HOMEDIR:
		{
			git_buf *out = va_arg(ap, git_buf *);
			git_str str = GIT_STR_INIT;
			const git_str *tmp;

			if ((error = git_buf_tostr(&str, out)) < 0 ||
			    (error = git_sysdir_get(&tmp, GIT_SYSDIR_HOME)) < 0 ||
			    (error = git_str_put(&str, tmp->ptr, tmp->size)) < 0)
				break;

			error = git_buf_fromstr(out, &str);
		}
		break;

	case GIT_OPT_SET_HOMEDIR:
		error = git_sysdir_set(GIT_SYSDIR_HOME, va_arg(ap, const char *));
		break;

	case GIT_OPT_GET_SERVER_CONNECT_TIMEOUT:
		*(va_arg(ap, int *)) = git_socket_stream__connect_timeout;
		break;

	case GIT_OPT_SET_SERVER_CONNECT_TIMEOUT:
		{
			int timeout = va_arg(ap, int);

			if (timeout < 0) {
				git_error_set(GIT_ERROR_INVALID, "invalid connect timeout");
				error = -1;
			} else {
				git_socket_stream__connect_timeout = timeout;
			}
		}
		break;

	case GIT_OPT_GET_SERVER_TIMEOUT:
		*(va_arg(ap, int *)) = git_socket_stream__timeout;
		break;

	case GIT_OPT_SET_SERVER_TIMEOUT:
		{
			int timeout = va_arg(ap, int);

			if (timeout < 0) {
				git_error_set(GIT_ERROR_INVALID, "invalid timeout");
				error = -1;
			} else {
				git_socket_stream__timeout = timeout;
			}
		}
		break;

	default:
		git_error_set(GIT_ERROR_INVALID, "invalid option key");
		error = -1;
	}

	va_end(ap);

	return error;
}
