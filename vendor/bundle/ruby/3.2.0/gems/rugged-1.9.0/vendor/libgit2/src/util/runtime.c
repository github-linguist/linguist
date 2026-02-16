/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2_util.h"
#include "runtime.h"

static git_runtime_shutdown_fn shutdown_callback[32];
static git_atomic32 shutdown_callback_count;

static git_atomic32 init_count;

static int init_common(git_runtime_init_fn init_fns[], size_t cnt)
{
	size_t i;
	int ret;

	/* Initialize subsystems that have global state */
	for (i = 0; i < cnt; i++) {
		if ((ret = init_fns[i]()) != 0)
			break;
	}

	GIT_MEMORY_BARRIER;

	return ret;
}

static void shutdown_common(void)
{
	git_runtime_shutdown_fn cb;
	int pos;

	for (pos = git_atomic32_get(&shutdown_callback_count);
	     pos > 0;
	     pos = git_atomic32_dec(&shutdown_callback_count)) {
		cb = git_atomic_swap(shutdown_callback[pos - 1], NULL);

		if (cb != NULL)
			cb();
	}
}

int git_runtime_shutdown_register(git_runtime_shutdown_fn callback)
{
	int count = git_atomic32_inc(&shutdown_callback_count);

	if (count > (int)ARRAY_SIZE(shutdown_callback) || count == 0) {
		git_error_set(GIT_ERROR_INVALID,
		              "too many shutdown callbacks registered");
		git_atomic32_dec(&shutdown_callback_count);
		return -1;
	}

	shutdown_callback[count - 1] = callback;

	return 0;
}

#if defined(GIT_THREADS) && defined(GIT_WIN32)

/*
 * On Win32, we use a spinlock to provide locking semantics.  This is
 * lighter-weight than a proper critical section.
 */
static volatile LONG init_spinlock = 0;

GIT_INLINE(int) init_lock(void)
{
	while (InterlockedCompareExchange(&init_spinlock, 1, 0)) { Sleep(0); }
	return 0;
}

GIT_INLINE(int) init_unlock(void)
{
	InterlockedExchange(&init_spinlock, 0);
	return 0;
}

#elif defined(GIT_THREADS) && defined(_POSIX_THREADS)

/*
 * On POSIX, we need to use a proper mutex for locking.  We might prefer
 * a spinlock here, too, but there's no static initializer for a
 * pthread_spinlock_t.
 */
static pthread_mutex_t init_mutex = PTHREAD_MUTEX_INITIALIZER;

GIT_INLINE(int) init_lock(void)
{
	return pthread_mutex_lock(&init_mutex) == 0 ? 0 : -1;
}

GIT_INLINE(int) init_unlock(void)
{
	return pthread_mutex_unlock(&init_mutex) == 0 ? 0 : -1;
}

#elif defined(GIT_THREADS)
# error unknown threading model
#else

# define init_lock() git__noop()
# define init_unlock() git__noop()

#endif

int git_runtime_init(git_runtime_init_fn init_fns[], size_t cnt)
{
	int ret;

	if (init_lock() < 0)
		return -1;

	/* Only do work on a 0 -> 1 transition of the refcount */
	if ((ret = git_atomic32_inc(&init_count)) == 1) {
		if (init_common(init_fns, cnt) < 0)
			ret = -1;
	}

	if (init_unlock() < 0)
		return -1;

	return ret;
}

int git_runtime_init_count(void)
{
	int ret;

	if (init_lock() < 0)
		return -1;

	ret = git_atomic32_get(&init_count);

	if (init_unlock() < 0)
		return -1;

	return ret;
}

int git_runtime_shutdown(void)
{
	int ret;

	/* Enter the lock */
	if (init_lock() < 0)
		return -1;

	/* Only do work on a 1 -> 0 transition of the refcount */
	if ((ret = git_atomic32_dec(&init_count)) == 0)
		shutdown_common();

	/* Exit the lock */
	if (init_unlock() < 0)
		return -1;

	return ret;
}
