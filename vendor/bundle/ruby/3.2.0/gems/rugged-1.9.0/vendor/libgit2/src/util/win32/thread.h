/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_win32_thread_h__
#define INCLUDE_win32_thread_h__

#include "git2_util.h"

#if defined (_MSC_VER)
#	define GIT_RESTRICT __restrict
#else
#	define GIT_RESTRICT __restrict__
#endif

typedef struct {
	HANDLE thread;
	void *(*proc)(void *);
	void *param;
	void *result;
} git_thread;

typedef CRITICAL_SECTION git_mutex;
typedef HANDLE git_cond;

typedef struct { void *Ptr; } GIT_SRWLOCK;

typedef struct {
	union {
		GIT_SRWLOCK srwl;
		CRITICAL_SECTION csec;
	} native;
} git_rwlock;

int git_threads_global_init(void);

int git_thread_create(git_thread *GIT_RESTRICT,
	void *(*) (void *),
	void *GIT_RESTRICT);
int git_thread_join(git_thread *, void **);
size_t git_thread_currentid(void);
void git_thread_exit(void *);

int git_mutex_init(git_mutex *GIT_RESTRICT mutex);
int git_mutex_free(git_mutex *);
int git_mutex_lock(git_mutex *);
int git_mutex_unlock(git_mutex *);

int git_cond_init(git_cond *);
int git_cond_free(git_cond *);
int git_cond_wait(git_cond *, git_mutex *);
int git_cond_signal(git_cond *);

int git_rwlock_init(git_rwlock *GIT_RESTRICT lock);
int git_rwlock_rdlock(git_rwlock *);
int git_rwlock_rdunlock(git_rwlock *);
int git_rwlock_wrlock(git_rwlock *);
int git_rwlock_wrunlock(git_rwlock *);
int git_rwlock_free(git_rwlock *);

#endif
