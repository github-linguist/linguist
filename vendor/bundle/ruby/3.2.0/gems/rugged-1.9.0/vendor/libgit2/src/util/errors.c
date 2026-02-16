/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2_util.h"

#include "errors.h"
#include "posix.h"
#include "str.h"
#include "runtime.h"

/*
 * Some static error data that is used when we're out of memory, TLS
 * has not been setup, or TLS has failed.
 */

static git_error oom_error = {
	"Out of memory",
	GIT_ERROR_NOMEMORY
};

static git_error uninitialized_error = {
	"library has not been initialized",
	GIT_ERROR_INVALID
};

static git_error tlsdata_error = {
	"thread-local data initialization failure",
	GIT_ERROR_THREAD
};

static git_error no_error = {
	"no error",
	GIT_ERROR_NONE
};

#define IS_STATIC_ERROR(err) \
	((err) == &oom_error || (err) == &uninitialized_error || \
	 (err) == &tlsdata_error || (err) == &no_error)

/* Per-thread error state (TLS) */

static git_tlsdata_key tls_key;

struct error_threadstate {
	/* The error message buffer. */
	git_str message;

	/* Error information, set by `git_error_set` and friends. */
	git_error error;

	/*
	 * The last error to occur; points to the error member of this
	 * struct _or_ a static error.
	 */
	git_error *last;
};

static void threadstate_dispose(struct error_threadstate *threadstate)
{
	if (!threadstate)
		return;

	git_str_dispose(&threadstate->message);
}

static struct error_threadstate *threadstate_get(void)
{
	struct error_threadstate *threadstate;

	if ((threadstate = git_tlsdata_get(tls_key)) != NULL)
		return threadstate;

	/*
	 * Avoid git__malloc here, since if it fails, it sets an error
	 * message, which requires thread state, which would allocate
	 * here, which would fail, which would set an error message...
	 */

	if ((threadstate = git__allocator.gmalloc(
			sizeof(struct error_threadstate),
			__FILE__, __LINE__)) == NULL)
		return NULL;

	memset(threadstate, 0, sizeof(struct error_threadstate));

	if (git_str_init(&threadstate->message, 0) < 0) {
		git__allocator.gfree(threadstate);
		return NULL;
	}

	git_tlsdata_set(tls_key, threadstate);
	return threadstate;
}

static void GIT_SYSTEM_CALL threadstate_free(void *threadstate)
{
	threadstate_dispose(threadstate);
	git__free(threadstate);
}

static void git_error_global_shutdown(void)
{
	struct error_threadstate *threadstate;

	threadstate = git_tlsdata_get(tls_key);
	git_tlsdata_set(tls_key, NULL);

	threadstate_dispose(threadstate);
	git__free(threadstate);

	git_tlsdata_dispose(tls_key);
}

int git_error_global_init(void)
{
	if (git_tlsdata_init(&tls_key, &threadstate_free) != 0)
		return -1;

	return git_runtime_shutdown_register(git_error_global_shutdown);
}

static void set_error_from_buffer(int error_class)
{
	struct error_threadstate *threadstate = threadstate_get();
	git_error *error;
	git_str *buf;

	if (!threadstate)
		return;

	error = &threadstate->error;
	buf = &threadstate->message;

	error->message = buf->ptr;
	error->klass = error_class;

	threadstate->last = error;
}

static void set_error(int error_class, char *string)
{
	struct error_threadstate *threadstate = threadstate_get();
	git_str *buf;

	if (!threadstate)
		return;

	buf = &threadstate->message;

	git_str_clear(buf);

	if (string)
		git_str_puts(buf, string);

	if (!git_str_oom(buf))
		set_error_from_buffer(error_class);
}

void git_error_set_oom(void)
{
	struct error_threadstate *threadstate = threadstate_get();

	if (!threadstate)
		return;

	threadstate->last = &oom_error;
}

void git_error_set(int error_class, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	git_error_vset(error_class, fmt, ap);
	va_end(ap);
}

void git_error_vset(int error_class, const char *fmt, va_list ap)
{
#ifdef GIT_WIN32
	DWORD win32_error_code = (error_class == GIT_ERROR_OS) ? GetLastError() : 0;
#endif

	struct error_threadstate *threadstate = threadstate_get();
	int error_code = (error_class == GIT_ERROR_OS) ? errno : 0;
	git_str *buf;

	if (!threadstate)
		return;

	buf = &threadstate->message;

	git_str_clear(buf);

	if (fmt) {
		git_str_vprintf(buf, fmt, ap);
		if (error_class == GIT_ERROR_OS)
			git_str_PUTS(buf, ": ");
	}

	if (error_class == GIT_ERROR_OS) {
#ifdef GIT_WIN32
		char *win32_error = git_win32_get_error_message(win32_error_code);
		if (win32_error) {
			git_str_puts(buf, win32_error);
			git__free(win32_error);

			SetLastError(0);
		}
		else
#endif
		if (error_code)
			git_str_puts(buf, strerror(error_code));

		if (error_code)
			errno = 0;
	}

	if (!git_str_oom(buf))
		set_error_from_buffer(error_class);
}

int git_error_set_str(int error_class, const char *string)
{
	struct error_threadstate *threadstate = threadstate_get();
	git_str *buf;

	GIT_ASSERT_ARG(string);

	if (!threadstate)
		return -1;

	buf = &threadstate->message;

	git_str_clear(buf);
	git_str_puts(buf, string);

	if (git_str_oom(buf))
		return -1;

	set_error_from_buffer(error_class);
	return 0;
}

void git_error_clear(void)
{
	struct error_threadstate *threadstate = threadstate_get();

	if (!threadstate)
		return;

	if (threadstate->last != NULL) {
		set_error(0, NULL);
		threadstate->last = NULL;
	}

	errno = 0;
#ifdef GIT_WIN32
	SetLastError(0);
#endif
}

bool git_error_exists(void)
{
	struct error_threadstate *threadstate;

	if ((threadstate = threadstate_get()) == NULL)
		return true;

	return threadstate->last != NULL;
}

const git_error *git_error_last(void)
{
	struct error_threadstate *threadstate;

	/* If the library is not initialized, return a static error. */
	if (!git_runtime_init_count())
		return &uninitialized_error;

	if ((threadstate = threadstate_get()) == NULL)
		return &tlsdata_error;

	if (!threadstate->last)
		return &no_error;

	return threadstate->last;
}

int git_error_save(git_error **out)
{
	struct error_threadstate *threadstate = threadstate_get();
	git_error *error, *dup;

	if (!threadstate) {
		*out = &tlsdata_error;
		return -1;
	}

	error = threadstate->last;

	if (!error || error == &no_error) {
		*out = &no_error;
		return 0;
	} else if (IS_STATIC_ERROR(error)) {
		*out = error;
		return 0;
	}

	if ((dup = git__malloc(sizeof(git_error))) == NULL) {
		*out = &oom_error;
		return -1;
	}

	dup->klass = error->klass;
	dup->message = git__strdup(error->message);

	if (!dup->message) {
		*out = &oom_error;
		return -1;
	}

	*out = dup;
	return 0;
}

int git_error_restore(git_error *error)
{
	struct error_threadstate *threadstate = threadstate_get();

	GIT_ASSERT_ARG(error);

	if (IS_STATIC_ERROR(error) && threadstate)
		threadstate->last = error;
	else
		set_error(error->klass, error->message);

	git_error_free(error);
	return 0;
}

void git_error_free(git_error *error)
{
	if (!error)
		return;

	if (IS_STATIC_ERROR(error))
		return;

	git__free(error->message);
	git__free(error);
}

int git_error_system_last(void)
{
#ifdef GIT_WIN32
	return GetLastError();
#else
	return errno;
#endif
}

void git_error_system_set(int code)
{
#ifdef GIT_WIN32
	SetLastError(code);
#else
	errno = code;
#endif
}

/* Deprecated error values and functions */

#ifndef GIT_DEPRECATE_HARD

#include "git2/deprecated.h"

const git_error *giterr_last(void)
{
	return git_error_last();
}

void giterr_clear(void)
{
	git_error_clear();
}

void giterr_set_str(int error_class, const char *string)
{
	git_error_set_str(error_class, string);
}

void giterr_set_oom(void)
{
	git_error_set_oom();
}
#endif
