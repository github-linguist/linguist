// Source: https://github.com/c3lang/c3c/blob/master/lib/std/threads/os/thread_win32.c3
// Permalink: https://github.com/c3lang/c3c/blob/edc55a2/lib/std/threads/os/thread_win32.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::thread::os @if(env::WIN32);
import std::os::win32, std::time;

distinct NativeThread = inline Win32_HANDLE;

struct NativeMutex
{
	union
	{
		Win32_CRITICAL_SECTION critical_section;
		Win32_HANDLE handle;
	}
	// Size is less than a Win32_HANDLE so due to alignment
	// there is no benefit to pack these into a bitstruct.
	bool already_locked;
	bool recursive;
	bool timed;
}

struct NativeOnceFlag
{
	int status;
	Win32_CRITICAL_SECTION lock;
}

struct NativeConditionVariable
{
	union
	{
		struct
		{
			Win32_HANDLE event_one;
			Win32_HANDLE event_all;
		}
		Win32_HANDLE[2] events;
	}
	uint waiters_count;
	Win32_CRITICAL_SECTION waiters_count_lock;
}

fn void! NativeMutex.init(&mtx, MutexType type)
{
	mtx.already_locked = false;
	mtx.recursive = (bool)(type & thread::MUTEX_RECURSIVE);
	mtx.timed = (bool)(type & thread::MUTEX_TIMED);
	if (!mtx.timed)
	{
		win32::initializeCriticalSection(&(mtx.critical_section));
		return;
	}
	if (!(mtx.handle = win32::createMutex(null, 0, null))) return ThreadFault.INIT_FAILED?;
}

fn void! NativeMutex.destroy(&mtx)
{
	if (!mtx.timed)
	{
		win32::deleteCriticalSection(&mtx.critical_section);
		return;
	}
	if (!win32::closeHandle(mtx.handle)) return ThreadFault.DESTROY_FAILED?;
}

fn void! NativeMutex.lock(&mtx)
{
	if (!mtx.timed)
	{
		win32::enterCriticalSection(&mtx.critical_section);
	}
	else
	{
		switch (win32::waitForSingleObject(mtx.handle, win32::INFINITE))
		{
			case win32::WAIT_OBJECT_0:
				break;
			case win32::WAIT_ABANDONED:
			default:
				return ThreadFault.LOCK_FAILED?;

		}
	}
	if (!mtx.recursive)
	{
		while (mtx.already_locked) win32::sleep(1);
	}
	mtx.already_locked = true;
}


/**
 * @require mtx.timed "Only available for timed locks"
 **/
fn void! NativeMutex.lock_timeout(&mtx, ulong ms)
{
	if (ms > uint.max) ms = uint.max;
	switch (win32::waitForSingleObject(mtx.handle, (uint)ms))
	{
		case win32::WAIT_OBJECT_0:
			break;
		case win32::WAIT_TIMEOUT:
			return ThreadFault.LOCK_TIMEOUT?;
		case win32::WAIT_ABANDONED:
		default:
			return ThreadFault.LOCK_FAILED?;
	}
	if (!mtx.recursive)
	{
		usz left_timeout = ms - 1;
		while (mtx.already_locked)
		{
			if (left_timeout-- == 0)
			{
				win32::releaseMutex(mtx.handle);
				return ThreadFault.LOCK_TIMEOUT?;
			}
			win32::sleep(1);
		}
		mtx.already_locked = true;
	}
}

fn bool NativeMutex.try_lock(&mtx)
{
	bool success = mtx.timed
		? win32::waitForSingleObject(mtx.handle, 0) == win32::WAIT_OBJECT_0
		: (bool)win32::tryEnterCriticalSection(&mtx.critical_section);

	if (!success) return false;
	if (!mtx.recursive)
	{
		if (mtx.already_locked)
		{
			if (mtx.timed)
			{
				win32::releaseMutex(mtx.handle);
			}
			else
			{
				win32::leaveCriticalSection(&mtx.critical_section);
			}
			return false;
		}
		mtx.already_locked = true;
	}
	return true;
}

fn void! NativeMutex.unlock(&mtx)
{
	mtx.already_locked = false;
	if (!mtx.timed)
	{
		win32::leaveCriticalSection(&mtx.critical_section);
		return;
	}
	if (!win32::releaseMutex(mtx.handle)) return ThreadFault.UNLOCK_FAILED?;
}

const int CONDITION_EVENT_ONE = 0;
const int CONDITION_EVENT_ALL = 1;

fn void! NativeConditionVariable.init(&cond)
{
	cond.waiters_count = 0;
	win32::initializeCriticalSection(&cond.waiters_count_lock);
	cond.event_one = win32::createEventA(null, 0, 0, null);
	if (!cond.event_one)
	{
		cond.event_all = (Win32_HANDLE)0;
		return ThreadFault.INIT_FAILED?;
	}
	cond.event_all = win32::createEventA(null, 1, 0, null);
	if (!cond.event_all)
	{
		win32::closeHandle(cond.event_one);
		cond.event_one = (Win32_HANDLE)0;
		return ThreadFault.INIT_FAILED?;
	}
}

fn void! NativeConditionVariable.destroy(&cond) @maydiscard
{
	if (cond.event_one) win32::closeHandle(cond.event_one);
	if (cond.event_all) win32::closeHandle(cond.event_all);
	win32::deleteCriticalSection(&cond.waiters_count_lock);
}

fn void! NativeConditionVariable.signal(&cond)
{
	win32::enterCriticalSection(&cond.waiters_count_lock);
	bool have_waiters = cond.waiters_count > 0;
	win32::leaveCriticalSection(&cond.waiters_count_lock);
	if (have_waiters && !win32::setEvent(cond.event_one)) return ThreadFault.SIGNAL_FAILED?;
}

fn void! NativeConditionVariable.broadcast(&cond)
{
	win32::enterCriticalSection(&cond.waiters_count_lock);
	bool have_waiters = cond.waiters_count > 0;
	win32::leaveCriticalSection(&cond.waiters_count_lock);
	if (have_waiters && !win32::setEvent(cond.event_all)) return ThreadFault.SIGNAL_FAILED?;
}

fn void! timedwait(NativeConditionVariable* cond, NativeMutex* mtx, uint timeout) @private
{
	win32::enterCriticalSection(&cond.waiters_count_lock);
	cond.waiters_count++;
	win32::leaveCriticalSection(&cond.waiters_count_lock);

	mtx.unlock()!;

	uint result = win32::waitForMultipleObjects(2, &cond.events, 0, timeout);
	switch (result)
	{
		case win32::WAIT_TIMEOUT:
			mtx.lock()!;
			return ThreadFault.WAIT_TIMEOUT?;
		case win32::WAIT_FAILED:
			mtx.lock()!;
			return ThreadFault.WAIT_FAILED?;
		default:
			break;
	}

	win32::enterCriticalSection(&cond.waiters_count_lock);
	cond.waiters_count--;
	// If event all && no waiters
	bool last_waiter = result == 1 && !cond.waiters_count;
	win32::leaveCriticalSection(&cond.waiters_count_lock);

	if (last_waiter)
	{
		if (!win32::resetEvent(cond.event_all))
		{
			mtx.lock()!;
			return ThreadFault.WAIT_FAILED?;
		}
	}

	mtx.lock()!;
}

fn void! NativeConditionVariable.wait(&cond, NativeMutex* mtx) @inline
{
	return timedwait(cond, mtx, win32::INFINITE) @inline;
}

fn void! NativeConditionVariable.wait_timeout(&cond, NativeMutex* mtx, ulong ms) @inline
{
	if (ms > uint.max) ms = uint.max;
	return timedwait(cond, mtx, (uint)ms) @inline;
}

fn void! NativeThread.create(&thread, ThreadFn func, void* args)
{
	if (!(*thread = (NativeThread)win32::createThread(null, 0, func, args, 0, null))) return ThreadFault.INIT_FAILED?;
}

fn void! NativeThread.detach(thread) @inline
{
	if (!win32::closeHandle(thread)) return ThreadFault.DETACH_FAILED?;
}


fn void native_thread_exit(int result) @inline
{
	win32::exitThread((uint)result);
}

fn void native_thread_yield()
{
	win32::sleep(0);
}

fn void NativeOnceFlag.call_once(&flag, OnceFn func)
{
	while (@volatile_load(flag.status) < 3)
	{
		switch (@volatile_load(flag.status))
		{
			case 0:
				if (mem::compare_exchange_volatile(&flag.status, 1, 0, AtomicOrdering.SEQ_CONSISTENT, AtomicOrdering.SEQ_CONSISTENT) == 0)
				{
					win32::initializeCriticalSection(&flag.lock);
					win32::enterCriticalSection(&flag.lock);
					@volatile_store(flag.status, 2);
					func();
					@volatile_store(flag.status, 3);
					win32::leaveCriticalSection(&flag.lock);
					return;
				}
				break;
			case 1:
				break;
			case 2:
				win32::enterCriticalSection(&flag.lock);
				win32::leaveCriticalSection(&flag.lock);
				break;
		}
	}
}

fn int! NativeThread.join(thread)
{
	uint res;
	if (win32::waitForSingleObject(thread, win32::INFINITE) == win32::WAIT_FAILED) return ThreadFault.JOIN_FAILED?;
	if (!win32::getExitCodeThread(thread, &res)) return ThreadFault.JOIN_FAILED?;
	defer win32::closeHandle(thread);
	return res;
}

fn NativeThread native_thread_current()
{
	return (NativeThread)win32::getCurrentThread();
}

fn bool NativeThread.equals(thread, NativeThread other)
{
	return win32::getThreadId(thread) == win32::getThreadId(other);
}

fn void! native_sleep_nano(NanoDuration ns)
{
	long ms = ns.to_ms();
	if (ms <= 0) return;
	if (ms > Win32_DWORD.max) ms = Win32_DWORD.max;
	if (win32::sleepEx((Win32_DWORD)ms, (Win32_BOOL)true) == win32::WAIT_IO_COMPLETION) return ThreadFault.INTERRUPTED?;
}
