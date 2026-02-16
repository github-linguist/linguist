/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_thread_h__
#define INCLUDE_thread_h__

#if defined(GIT_THREADS)

#if defined(__clang__)

# if (__clang_major__ < 3 || (__clang_major__ == 3 && __clang_minor__ < 1))
#  error Atomic primitives do not exist on this version of clang; configure libgit2 with -DUSE_THREADS=OFF
# else
#  define GIT_BUILTIN_ATOMIC
# endif

#elif defined(__GNUC__)

# if (__GNUC__ < 4 || (__GNUC__ == 4 && __GNUC_MINOR__ < 1))
#  error Atomic primitives do not exist on this version of gcc; configure libgit2 with -DUSE_THREADS=OFF
# elif (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 7))
#  define GIT_BUILTIN_ATOMIC
# else
#  define GIT_BUILTIN_SYNC
# endif

#endif

#endif /* GIT_THREADS */

/* Common operations even if threading has been disabled */
typedef struct {
#if defined(GIT_WIN32)
	volatile long val;
#else
	volatile int val;
#endif
} git_atomic32;

#ifdef GIT_ARCH_64

typedef struct {
#if defined(GIT_WIN32)
	volatile __int64 val;
#else
	volatile int64_t val;
#endif
} git_atomic64;

typedef git_atomic64 git_atomic_ssize;

#define git_atomic_ssize_set git_atomic64_set
#define git_atomic_ssize_add git_atomic64_add
#define git_atomic_ssize_get git_atomic64_get

#else

typedef git_atomic32 git_atomic_ssize;

#define git_atomic_ssize_set git_atomic32_set
#define git_atomic_ssize_add git_atomic32_add
#define git_atomic_ssize_get git_atomic32_get

#endif

#ifdef GIT_THREADS

#ifdef GIT_WIN32
#   include "win32/thread.h"
#else
#   include "unix/pthread.h"
#endif

/*
 * Atomically sets the contents of *a to be val.
 */
GIT_INLINE(void) git_atomic32_set(git_atomic32 *a, int val)
{
#if defined(GIT_WIN32)
	InterlockedExchange(&a->val, (LONG)val);
#elif defined(GIT_BUILTIN_ATOMIC)
	__atomic_store_n(&a->val, val, __ATOMIC_SEQ_CST);
#elif defined(GIT_BUILTIN_SYNC)
	__sync_lock_test_and_set(&a->val, val);
#else
#	error "Unsupported architecture for atomic operations"
#endif
}

/*
 * Atomically increments the contents of *a by 1, and stores the result back into *a.
 * @return the result of the operation.
 */
GIT_INLINE(int) git_atomic32_inc(git_atomic32 *a)
{
#if defined(GIT_WIN32)
	return InterlockedIncrement(&a->val);
#elif defined(GIT_BUILTIN_ATOMIC)
	return __atomic_add_fetch(&a->val, 1, __ATOMIC_SEQ_CST);
#elif defined(GIT_BUILTIN_SYNC)
	return __sync_add_and_fetch(&a->val, 1);
#else
#	error "Unsupported architecture for atomic operations"
#endif
}

/*
 * Atomically adds the contents of *a and addend, and stores the result back into *a.
 * @return the result of the operation.
 */
GIT_INLINE(int) git_atomic32_add(git_atomic32 *a, int32_t addend)
{
#if defined(GIT_WIN32)
	return InterlockedAdd(&a->val, addend);
#elif defined(GIT_BUILTIN_ATOMIC)
	return __atomic_add_fetch(&a->val, addend, __ATOMIC_SEQ_CST);
#elif defined(GIT_BUILTIN_SYNC)
	return __sync_add_and_fetch(&a->val, addend);
#else
#	error "Unsupported architecture for atomic operations"
#endif
}

/*
 * Atomically decrements the contents of *a by 1, and stores the result back into *a.
 * @return the result of the operation.
 */
GIT_INLINE(int) git_atomic32_dec(git_atomic32 *a)
{
#if defined(GIT_WIN32)
	return InterlockedDecrement(&a->val);
#elif defined(GIT_BUILTIN_ATOMIC)
	return __atomic_sub_fetch(&a->val, 1, __ATOMIC_SEQ_CST);
#elif defined(GIT_BUILTIN_SYNC)
	return __sync_sub_and_fetch(&a->val, 1);
#else
#	error "Unsupported architecture for atomic operations"
#endif
}

/*
 * Atomically gets the contents of *a.
 * @return the contents of *a.
 */
GIT_INLINE(int) git_atomic32_get(git_atomic32 *a)
{
#if defined(GIT_WIN32)
	return (int)InterlockedCompareExchange(&a->val, 0, 0);
#elif defined(GIT_BUILTIN_ATOMIC)
	return __atomic_load_n(&a->val, __ATOMIC_SEQ_CST);
#elif defined(GIT_BUILTIN_SYNC)
	return __sync_val_compare_and_swap(&a->val, 0, 0);
#else
#	error "Unsupported architecture for atomic operations"
#endif
}

GIT_INLINE(void *) git_atomic__compare_and_swap(
	void * volatile *ptr, void *oldval, void *newval)
{
#if defined(GIT_WIN32)
	return InterlockedCompareExchangePointer((volatile PVOID *)ptr, newval, oldval);
#elif defined(GIT_BUILTIN_ATOMIC)
	void *foundval = oldval;
	__atomic_compare_exchange(ptr, &foundval, &newval, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
	return foundval;
#elif defined(GIT_BUILTIN_SYNC)
	return __sync_val_compare_and_swap(ptr, oldval, newval);
#else
#	error "Unsupported architecture for atomic operations"
#endif
}

GIT_INLINE(volatile void *) git_atomic__swap(
	void * volatile *ptr, void *newval)
{
#if defined(GIT_WIN32)
	return InterlockedExchangePointer(ptr, newval);
#elif defined(GIT_BUILTIN_ATOMIC)
	void * foundval = NULL;
	__atomic_exchange(ptr, &newval, &foundval, __ATOMIC_SEQ_CST);
	return foundval;
#elif defined(GIT_BUILTIN_SYNC)
	return (volatile void *)__sync_lock_test_and_set(ptr, newval);
#else
#	error "Unsupported architecture for atomic operations"
#endif
}

GIT_INLINE(volatile void *) git_atomic__load(void * volatile *ptr)
{
#if defined(GIT_WIN32)
	void *newval = NULL, *oldval = NULL;
	return (volatile void *)InterlockedCompareExchangePointer((volatile PVOID *)ptr, newval, oldval);
#elif defined(GIT_BUILTIN_ATOMIC)
	return (volatile void *)__atomic_load_n(ptr, __ATOMIC_SEQ_CST);
#elif defined(GIT_BUILTIN_SYNC)
	return (volatile void *)__sync_val_compare_and_swap(ptr, 0, 0);
#else
#	error "Unsupported architecture for atomic operations"
#endif
}

#ifdef GIT_ARCH_64

/*
 * Atomically adds the contents of *a and addend, and stores the result back into *a.
 * @return the result of the operation.
 */
GIT_INLINE(int64_t) git_atomic64_add(git_atomic64 *a, int64_t addend)
{
#if defined(GIT_WIN32)
	return InterlockedAdd64(&a->val, addend);
#elif defined(GIT_BUILTIN_ATOMIC)
	return __atomic_add_fetch(&a->val, addend, __ATOMIC_SEQ_CST);
#elif defined(GIT_BUILTIN_SYNC)
	return __sync_add_and_fetch(&a->val, addend);
#else
#	error "Unsupported architecture for atomic operations"
#endif
}

/*
 * Atomically sets the contents of *a to be val.
 */
GIT_INLINE(void) git_atomic64_set(git_atomic64 *a, int64_t val)
{
#if defined(GIT_WIN32)
	InterlockedExchange64(&a->val, val);
#elif defined(GIT_BUILTIN_ATOMIC)
	__atomic_store_n(&a->val, val, __ATOMIC_SEQ_CST);
#elif defined(GIT_BUILTIN_SYNC)
	__sync_lock_test_and_set(&a->val, val);
#else
#	error "Unsupported architecture for atomic operations"
#endif
}

/*
 * Atomically gets the contents of *a.
 * @return the contents of *a.
 */
GIT_INLINE(int64_t) git_atomic64_get(git_atomic64 *a)
{
#if defined(GIT_WIN32)
	return (int64_t)InterlockedCompareExchange64(&a->val, 0, 0);
#elif defined(GIT_BUILTIN_ATOMIC)
	return __atomic_load_n(&a->val, __ATOMIC_SEQ_CST);
#elif defined(GIT_BUILTIN_SYNC)
	return __sync_val_compare_and_swap(&a->val, 0, 0);
#else
#	error "Unsupported architecture for atomic operations"
#endif
}

#endif

#else

#define git_threads_global_init	git__noop

#define git_thread unsigned int
#define git_thread_create(t, s, a) git__noop(t, s, a)
#define git_thread_join(i, s)	git__noop_args(i, s)

/* Pthreads Mutex */
#define git_mutex unsigned int
#define git_mutex_init(a)	git__noop_args(a)
#define git_mutex_init(a)	git__noop_args(a)
#define git_mutex_lock(a)	git__noop_args(a)
#define git_mutex_unlock(a)	git__noop_args(a)
#define git_mutex_free(a)	git__noop_args(a)

/* Pthreads condition vars */
#define git_cond unsigned int
#define git_cond_init(c)	git__noop_args(c)
#define git_cond_free(c)	git__noop_args(c)
#define git_cond_wait(c, l)	git__noop_args(c, l)
#define git_cond_signal(c)	git__noop_args(c)
#define git_cond_broadcast(c)	git__noop_args(c)

/* Pthreads rwlock */
#define git_rwlock unsigned int
#define git_rwlock_init(a)	git__noop_args(a)
#define git_rwlock_rdlock(a)	git__noop_args(a)
#define git_rwlock_rdunlock(a)	git__noop_args(a)
#define git_rwlock_wrlock(a)	git__noop_args(a)
#define git_rwlock_wrunlock(a)	git__noop_args(a)
#define git_rwlock_free(a)	git__noop_args(a)

#define GIT_RWLOCK_STATIC_INIT	0


GIT_INLINE(void) git_atomic32_set(git_atomic32 *a, int val)
{
	a->val = val;
}

GIT_INLINE(int) git_atomic32_inc(git_atomic32 *a)
{
	return ++a->val;
}

GIT_INLINE(int) git_atomic32_add(git_atomic32 *a, int32_t addend)
{
	a->val += addend;
	return a->val;
}

GIT_INLINE(int) git_atomic32_dec(git_atomic32 *a)
{
	return --a->val;
}

GIT_INLINE(int) git_atomic32_get(git_atomic32 *a)
{
	return (int)a->val;
}

GIT_INLINE(void *) git_atomic__compare_and_swap(
	void * volatile *ptr, void *oldval, void *newval)
{
	void *foundval = *ptr;
	if (foundval == oldval)
		*ptr = newval;
	return foundval;
}

GIT_INLINE(volatile void *) git_atomic__swap(
	void * volatile *ptr, void *newval)
{
	volatile void *old = *ptr;
	*ptr = newval;
	return old;
}

GIT_INLINE(volatile void *) git_atomic__load(void * volatile *ptr)
{
	return *ptr;
}

#ifdef GIT_ARCH_64

GIT_INLINE(int64_t) git_atomic64_add(git_atomic64 *a, int64_t addend)
{
	a->val += addend;
	return a->val;
}

GIT_INLINE(void) git_atomic64_set(git_atomic64 *a, int64_t val)
{
	a->val = val;
}

GIT_INLINE(int64_t) git_atomic64_get(git_atomic64 *a)
{
	return (int64_t)a->val;
}

#endif

#endif

/*
 * Atomically replace the contents of *ptr (if they are equal to oldval) with
 * newval. ptr must point to a pointer or a value that is the same size as a
 * pointer. This is semantically compatible with:
 *
 *   #define git_atomic_compare_and_swap(ptr, oldval, newval) \
 *   ({                                                       \
 *       void *foundval = *ptr;                               \
 *       if (foundval == oldval)                              \
 *           *ptr = newval;                                   \
 *       foundval;                                            \
 *   })
 *
 * @return the original contents of *ptr.
 */
#define git_atomic_compare_and_swap(ptr, oldval, newval) \
	git_atomic__compare_and_swap((void * volatile *)ptr, oldval, newval)

/*
 * Atomically replace the contents of v with newval. v must be the same size as
 * a pointer. This is semantically compatible with:
 *
 *   #define git_atomic_swap(v, newval) \
 *   ({                                 \
 *       volatile void *old = v;        \
 *       v = newval;                    \
 *       old;                           \
 *   })
 *
 * @return the original contents of v.
 */
#define git_atomic_swap(v, newval) \
	(void *)git_atomic__swap((void * volatile *)&(v), newval)

/*
 * Atomically reads the contents of v. v must be the same size as a pointer.
 * This is semantically compatible with:
 *
 *   #define git_atomic_load(v) v
 *
 * @return the contents of v.
 */
#define git_atomic_load(v) \
	(void *)git_atomic__load((void * volatile *)&(v))

#if defined(GIT_THREADS)

# if defined(GIT_WIN32)
#  define GIT_MEMORY_BARRIER MemoryBarrier()
# elif defined(GIT_BUILTIN_ATOMIC)
#  define GIT_MEMORY_BARRIER __atomic_thread_fence(__ATOMIC_SEQ_CST)
# elif defined(GIT_BUILTIN_SYNC)
#  define GIT_MEMORY_BARRIER __sync_synchronize()
# endif

#else

# define GIT_MEMORY_BARRIER /* noop */

#endif

/* Thread-local data */

#if !defined(GIT_THREADS)
# define git_tlsdata_key int
#elif defined(GIT_WIN32)
# define git_tlsdata_key DWORD
#elif defined(_POSIX_THREADS)
# define git_tlsdata_key pthread_key_t
#else
# error unknown threading model
#endif

/**
 * Create a thread-local data key.  The destroy function will be
 * called upon thread exit.  On some platforms, it may be called
 * when all threads have deleted their keys.
 *
 * Note that the tlsdata functions do not set an error message on
 * failure; this is because the error handling in libgit2 is itself
 * handled by thread-local data storage.
 *
 * @param key the tlsdata key
 * @param destroy_fn function pointer called upon thread exit
 * @return 0 on success, non-zero on failure
 */
int git_tlsdata_init(git_tlsdata_key *key, void (GIT_SYSTEM_CALL *destroy_fn)(void *));

/**
 * Set a the thread-local value for the given key.
 *
 * @param key the tlsdata key to store data on
 * @param value the pointer to store
 * @return 0 on success, non-zero on failure
 */
int git_tlsdata_set(git_tlsdata_key key, void *value);

/**
 * Get the thread-local value for the given key.
 *
 * @param key the tlsdata key to retrieve the value of
 * @return the pointer stored with git_tlsdata_set
 */
void *git_tlsdata_get(git_tlsdata_key key);

/**
 * Delete the given thread-local key.
 *
 * @param key the tlsdata key to dispose
 * @return 0 on success, non-zero on failure
 */
int git_tlsdata_dispose(git_tlsdata_key key);

#endif
