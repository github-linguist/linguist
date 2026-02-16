/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "streams/registry.h"

#include "runtime.h"
#include "streams/tls.h"
#include "streams/mbedtls.h"
#include "streams/openssl.h"
#include "streams/stransport.h"

struct stream_registry {
	git_rwlock lock;
	git_stream_registration callbacks;
	git_stream_registration tls_callbacks;
};

static struct stream_registry stream_registry;

static void shutdown_stream_registry(void)
{
	git_rwlock_free(&stream_registry.lock);
}

int git_stream_registry_global_init(void)
{
	if (git_rwlock_init(&stream_registry.lock) < 0)
		return -1;

	return git_runtime_shutdown_register(shutdown_stream_registry);
}

GIT_INLINE(void) stream_registration_cpy(
	git_stream_registration *target,
	git_stream_registration *src)
{
	if (src)
		memcpy(target, src, sizeof(git_stream_registration));
	else
		memset(target, 0, sizeof(git_stream_registration));
}

int git_stream_registry_lookup(git_stream_registration *out, git_stream_t type)
{
	git_stream_registration *target;
	int error = GIT_ENOTFOUND;

	GIT_ASSERT_ARG(out);

	switch(type) {
	case GIT_STREAM_STANDARD:
		target = &stream_registry.callbacks;
		break;
	case GIT_STREAM_TLS:
		target = &stream_registry.tls_callbacks;
		break;
	default:
		git_error_set(GIT_ERROR_INVALID, "invalid stream type");
		return -1;
	}

	if (git_rwlock_rdlock(&stream_registry.lock) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to lock stream registry");
		return -1;
	}

	if (target->init) {
		stream_registration_cpy(out, target);
		error = 0;
	}

	git_rwlock_rdunlock(&stream_registry.lock);
	return error;
}

int git_stream_register(git_stream_t type, git_stream_registration *registration)
{
	GIT_ASSERT(!registration || registration->init);

	GIT_ERROR_CHECK_VERSION(registration, GIT_STREAM_VERSION, "stream_registration");

	if (git_rwlock_wrlock(&stream_registry.lock) < 0) {
		git_error_set(GIT_ERROR_OS, "failed to lock stream registry");
		return -1;
	}

	if ((type & GIT_STREAM_STANDARD) == GIT_STREAM_STANDARD)
		stream_registration_cpy(&stream_registry.callbacks, registration);

	if ((type & GIT_STREAM_TLS) == GIT_STREAM_TLS)
		stream_registration_cpy(&stream_registry.tls_callbacks, registration);

	git_rwlock_wrunlock(&stream_registry.lock);
	return 0;
}

#ifndef GIT_DEPRECATE_HARD
int git_stream_register_tls(
	int GIT_CALLBACK(ctor)(git_stream **out, const char *host, const char *port))
{
	git_stream_registration registration = {0};

	if (ctor) {
		registration.version = GIT_STREAM_VERSION;
		registration.init = ctor;
		registration.wrap = NULL;

		return git_stream_register(GIT_STREAM_TLS, &registration);
	} else {
		return git_stream_register(GIT_STREAM_TLS, NULL);
	}
}
#endif
