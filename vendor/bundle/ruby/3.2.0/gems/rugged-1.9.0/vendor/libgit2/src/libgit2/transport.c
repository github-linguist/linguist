/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "git2/types.h"
#include "git2/remote.h"
#include "git2/net.h"
#include "git2/transport.h"
#include "git2/sys/transport.h"
#include "fs_path.h"

typedef struct transport_definition {
	char *prefix;
	git_transport_cb fn;
	void *param;
} transport_definition;

static git_smart_subtransport_definition http_subtransport_definition = { git_smart_subtransport_http, 1, NULL };
static git_smart_subtransport_definition git_subtransport_definition = { git_smart_subtransport_git, 0, NULL };

#ifdef GIT_SSH
static git_smart_subtransport_definition ssh_subtransport_definition = { git_smart_subtransport_ssh, 0, NULL };
#endif

static transport_definition local_transport_definition = { "file://", git_transport_local, NULL };

static transport_definition transports[] = {
	{ "git://",   git_transport_smart, &git_subtransport_definition },
	{ "http://",  git_transport_smart, &http_subtransport_definition },
	{ "https://", git_transport_smart, &http_subtransport_definition },
	{ "file://",  git_transport_local, NULL },

#ifdef GIT_SSH
	{ "ssh://",   git_transport_smart, &ssh_subtransport_definition },
	{ "ssh+git://",   git_transport_smart, &ssh_subtransport_definition },
	{ "git+ssh://",   git_transport_smart, &ssh_subtransport_definition },
#endif

	{ NULL, 0, 0 }
};

static git_vector custom_transports = GIT_VECTOR_INIT;

#define GIT_TRANSPORT_COUNT (sizeof(transports)/sizeof(transports[0])) - 1

static transport_definition * transport_find_by_url(const char *url)
{
	size_t i = 0;
	transport_definition *d;

	/* Find a user transport who wants to deal with this URI */
	git_vector_foreach(&custom_transports, i, d) {
		if (strncasecmp(url, d->prefix, strlen(d->prefix)) == 0) {
			return d;
		}
	}

	/* Find a system transport for this URI */
	for (i = 0; i < GIT_TRANSPORT_COUNT; ++i) {
		d = &transports[i];

		if (strncasecmp(url, d->prefix, strlen(d->prefix)) == 0) {
			return d;
		}
	}

	return NULL;
}

static int transport_find_fn(
	git_transport_cb *out,
	const char *url,
	void **param)
{
	transport_definition *definition = transport_find_by_url(url);

#ifdef GIT_WIN32
	/* On Windows, it might not be possible to discern between absolute local
	 * and ssh paths - first check if this is a valid local path that points
	 * to a directory and if so assume local path, else assume SSH */

	/* Check to see if the path points to a file on the local file system */
	if (!definition && git_fs_path_exists(url) && git_fs_path_isdir(url))
		definition = &local_transport_definition;
#endif

	/* For other systems, perform the SSH check first, to avoid going to the
	 * filesystem if it is not necessary */

	/* It could be a SSH remote path. Check to see if there's a : */
	if (!definition && strrchr(url, ':')) {
		/* re-search transports again with ssh:// as url
		 * so that we can find a third party ssh transport */
		definition = transport_find_by_url("ssh://");
	}

#ifndef GIT_WIN32
	/* Check to see if the path points to a file on the local file system */
	if (!definition && git_fs_path_exists(url) && git_fs_path_isdir(url))
		definition = &local_transport_definition;
#endif

	if (!definition)
		return GIT_ENOTFOUND;

	*out = definition->fn;
	*param = definition->param;

	return 0;
}

/**************
 * Public API *
 **************/

int git_transport_new(git_transport **out, git_remote *owner, const char *url)
{
	git_transport_cb fn;
	git_transport *transport;
	void *param;
	int error;

	if ((error = transport_find_fn(&fn, url, &param)) == GIT_ENOTFOUND) {
		git_error_set(GIT_ERROR_NET, "unsupported URL protocol");
		return -1;
	} else if (error < 0)
		return error;

	if ((error = fn(&transport, owner, param)) < 0)
		return error;

	GIT_ERROR_CHECK_VERSION(transport, GIT_TRANSPORT_VERSION, "git_transport");

	*out = transport;

	return 0;
}

int git_transport_register(
	const char *scheme,
	git_transport_cb cb,
	void *param)
{
	git_str prefix = GIT_STR_INIT;
	transport_definition *d, *definition = NULL;
	size_t i;
	int error = 0;

	GIT_ASSERT_ARG(scheme);
	GIT_ASSERT_ARG(cb);

	if ((error = git_str_printf(&prefix, "%s://", scheme)) < 0)
		goto on_error;

	git_vector_foreach(&custom_transports, i, d) {
		if (strcasecmp(d->prefix, prefix.ptr) == 0) {
			error = GIT_EEXISTS;
			goto on_error;
		}
	}

	definition = git__calloc(1, sizeof(transport_definition));
	GIT_ERROR_CHECK_ALLOC(definition);

	definition->prefix = git_str_detach(&prefix);
	definition->fn = cb;
	definition->param = param;

	if (git_vector_insert(&custom_transports, definition) < 0)
		goto on_error;

	return 0;

on_error:
	git_str_dispose(&prefix);
	git__free(definition);
	return error;
}

int git_transport_unregister(const char *scheme)
{
	git_str prefix = GIT_STR_INIT;
	transport_definition *d;
	size_t i;
	int error = 0;

	GIT_ASSERT_ARG(scheme);

	if ((error = git_str_printf(&prefix, "%s://", scheme)) < 0)
		goto done;

	git_vector_foreach(&custom_transports, i, d) {
		if (strcasecmp(d->prefix, prefix.ptr) == 0) {
			if ((error = git_vector_remove(&custom_transports, i)) < 0)
				goto done;

			git__free(d->prefix);
			git__free(d);

			if (!custom_transports.length)
				git_vector_dispose(&custom_transports);

			error = 0;
			goto done;
		}
	}

	error = GIT_ENOTFOUND;

done:
	git_str_dispose(&prefix);
	return error;
}

int git_transport_init(git_transport *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_transport, GIT_TRANSPORT_INIT);
	return 0;
}
