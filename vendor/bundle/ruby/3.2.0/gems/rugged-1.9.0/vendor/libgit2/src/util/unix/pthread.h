/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_unix_pthread_h__
#define INCLUDE_unix_pthread_h__

typedef struct {
	pthread_t thread;
} git_thread;

GIT_INLINE(int) git_threads_global_init(void) { return 0; }

#define git_thread_create(git_thread_ptr, start_routine, arg) \
	pthread_create(&(git_thread_ptr)->thread, NULL, start_routine, arg)
#define git_thread_join(git_thread_ptr, status) \
	pthread_join((git_thread_ptr)->thread, status)
#define git_thread_currentid() ((size_t)(pthread_self()))
#define git_thread_exit(retval) pthread_exit(retval)

/* Git Mutex */
#define git_mutex pthread_mutex_t
#define git_mutex_init(a)	pthread_mutex_init(a, NULL)
#define git_mutex_lock(a)	pthread_mutex_lock(a)
#define git_mutex_unlock(a)     pthread_mutex_unlock(a)
#define git_mutex_free(a)	pthread_mutex_destroy(a)

/* Git condition vars */
#define git_cond pthread_cond_t
#define git_cond_init(c)	pthread_cond_init(c, NULL)
#define git_cond_free(c) 	pthread_cond_destroy(c)
#define git_cond_wait(c, l)	pthread_cond_wait(c, l)
#define git_cond_signal(c)	pthread_cond_signal(c)
#define git_cond_broadcast(c)	pthread_cond_broadcast(c)

/* Pthread (-ish) rwlock
 *
 * This differs from normal pthreads rwlocks in two ways:
 * 1. Separate APIs for releasing read locks and write locks (as
 *    opposed to the pure POSIX API which only has one unlock fn)
 * 2. You should not use recursive read locks (i.e. grabbing a read
 *    lock in a thread that already holds a read lock) because the
 *    Windows implementation doesn't support it
 */
#define git_rwlock              pthread_rwlock_t
#define git_rwlock_init(a)	pthread_rwlock_init(a, NULL)
#define git_rwlock_rdlock(a)	pthread_rwlock_rdlock(a)
#define git_rwlock_rdunlock(a)	pthread_rwlock_unlock(a)
#define git_rwlock_wrlock(a)	pthread_rwlock_wrlock(a)
#define git_rwlock_wrunlock(a)	pthread_rwlock_unlock(a)
#define git_rwlock_free(a)	pthread_rwlock_destroy(a)
#define GIT_RWLOCK_STATIC_INIT	PTHREAD_RWLOCK_INITIALIZER

#endif
