/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "thread.h"
#include "runtime.h"

#define CLEAN_THREAD_EXIT 0x6F012842

typedef void (WINAPI *win32_srwlock_fn)(GIT_SRWLOCK *);

static win32_srwlock_fn win32_srwlock_initialize;
static win32_srwlock_fn win32_srwlock_acquire_shared;
static win32_srwlock_fn win32_srwlock_release_shared;
static win32_srwlock_fn win32_srwlock_acquire_exclusive;
static win32_srwlock_fn win32_srwlock_release_exclusive;

static DWORD fls_index;

/* The thread procedure stub used to invoke the caller's procedure
 * and capture the return value for later collection. Windows will
 * only hold a DWORD, but we need to be able to store an entire
 * void pointer. This requires the indirection. */
static DWORD WINAPI git_win32__threadproc(LPVOID lpParameter)
{
	git_thread *thread = lpParameter;

	/* Set the current thread for `git_thread_exit` */
	FlsSetValue(fls_index, thread);

	thread->result = thread->proc(thread->param);

	return CLEAN_THREAD_EXIT;
}

static void git_threads_global_shutdown(void)
{
	FlsFree(fls_index);
}

int git_threads_global_init(void)
{
	HMODULE hModule = GetModuleHandleW(L"kernel32");

	if (hModule) {
		win32_srwlock_initialize = (win32_srwlock_fn)(void *)
			GetProcAddress(hModule, "InitializeSRWLock");
		win32_srwlock_acquire_shared = (win32_srwlock_fn)(void *)
			GetProcAddress(hModule, "AcquireSRWLockShared");
		win32_srwlock_release_shared = (win32_srwlock_fn)(void *)
			GetProcAddress(hModule, "ReleaseSRWLockShared");
		win32_srwlock_acquire_exclusive = (win32_srwlock_fn)(void *)
			GetProcAddress(hModule, "AcquireSRWLockExclusive");
		win32_srwlock_release_exclusive = (win32_srwlock_fn)(void *)
			GetProcAddress(hModule, "ReleaseSRWLockExclusive");
	}

	if ((fls_index = FlsAlloc(NULL)) == FLS_OUT_OF_INDEXES)
		return -1;

	return git_runtime_shutdown_register(git_threads_global_shutdown);
}

int git_thread_create(
	git_thread *GIT_RESTRICT thread,
	void *(*start_routine)(void*),
	void *GIT_RESTRICT arg)
{
	thread->result = NULL;
	thread->param = arg;
	thread->proc = start_routine;
	thread->thread = CreateThread(
		NULL, 0, git_win32__threadproc, thread, 0, NULL);

	return thread->thread ? 0 : -1;
}

int git_thread_join(
	git_thread *thread,
	void **value_ptr)
{
	DWORD exit;

	if (WaitForSingleObject(thread->thread, INFINITE) != WAIT_OBJECT_0)
		return -1;

	if (!GetExitCodeThread(thread->thread, &exit)) {
		CloseHandle(thread->thread);
		return -1;
	}

	/* Check for the thread having exited uncleanly. If exit was unclean,
	 * then we don't have a return value to give back to the caller. */
	GIT_ASSERT(exit == CLEAN_THREAD_EXIT);

	if (value_ptr)
		*value_ptr = thread->result;

	CloseHandle(thread->thread);
	return 0;
}

void git_thread_exit(void *value)
{
	git_thread *thread = FlsGetValue(fls_index);

	if (thread)
		thread->result = value;

	ExitThread(CLEAN_THREAD_EXIT);
}

size_t git_thread_currentid(void)
{
	return GetCurrentThreadId();
}

int git_mutex_init(git_mutex *GIT_RESTRICT mutex)
{
	InitializeCriticalSection(mutex);
	return 0;
}

int git_mutex_free(git_mutex *mutex)
{
	DeleteCriticalSection(mutex);
	return 0;
}

int git_mutex_lock(git_mutex *mutex)
{
	EnterCriticalSection(mutex);
	return 0;
}

int git_mutex_unlock(git_mutex *mutex)
{
	LeaveCriticalSection(mutex);
	return 0;
}

int git_cond_init(git_cond *cond)
{
	/* This is an auto-reset event. */
	*cond = CreateEventW(NULL, FALSE, FALSE, NULL);
	GIT_ASSERT(*cond);

	/* If we can't create the event, claim that the reason was out-of-memory.
	 * The actual reason can be fetched with GetLastError(). */
	return *cond ? 0 : ENOMEM;
}

int git_cond_free(git_cond *cond)
{
	BOOL closed;

	if (!cond)
		return EINVAL;

	closed = CloseHandle(*cond);
	GIT_ASSERT(closed);
	GIT_UNUSED(closed);

	*cond = NULL;
	return 0;
}

int git_cond_wait(git_cond *cond, git_mutex *mutex)
{
	int error;
	DWORD wait_result;

	if (!cond || !mutex)
		return EINVAL;

	/* The caller must be holding the mutex. */
	error = git_mutex_unlock(mutex);

	if (error)
		return error;

	wait_result = WaitForSingleObject(*cond, INFINITE);
	GIT_ASSERT(WAIT_OBJECT_0 == wait_result);
	GIT_UNUSED(wait_result);

	return git_mutex_lock(mutex);
}

int git_cond_signal(git_cond *cond)
{
	BOOL signaled;

	if (!cond)
		return EINVAL;

	signaled = SetEvent(*cond);
	GIT_ASSERT(signaled);
	GIT_UNUSED(signaled);

	return 0;
}

int git_rwlock_init(git_rwlock *GIT_RESTRICT lock)
{
	if (win32_srwlock_initialize)
		win32_srwlock_initialize(&lock->native.srwl);
	else
		InitializeCriticalSection(&lock->native.csec);

	return 0;
}

int git_rwlock_rdlock(git_rwlock *lock)
{
	if (win32_srwlock_acquire_shared)
		win32_srwlock_acquire_shared(&lock->native.srwl);
	else
		EnterCriticalSection(&lock->native.csec);

	return 0;
}

int git_rwlock_rdunlock(git_rwlock *lock)
{
	if (win32_srwlock_release_shared)
		win32_srwlock_release_shared(&lock->native.srwl);
	else
		LeaveCriticalSection(&lock->native.csec);

	return 0;
}

int git_rwlock_wrlock(git_rwlock *lock)
{
	if (win32_srwlock_acquire_exclusive)
		win32_srwlock_acquire_exclusive(&lock->native.srwl);
	else
		EnterCriticalSection(&lock->native.csec);

	return 0;
}

int git_rwlock_wrunlock(git_rwlock *lock)
{
	if (win32_srwlock_release_exclusive)
		win32_srwlock_release_exclusive(&lock->native.srwl);
	else
		LeaveCriticalSection(&lock->native.csec);

	return 0;
}

int git_rwlock_free(git_rwlock *lock)
{
	if (!win32_srwlock_initialize)
		DeleteCriticalSection(&lock->native.csec);
	git__memzero(lock, sizeof(*lock));
	return 0;
}
