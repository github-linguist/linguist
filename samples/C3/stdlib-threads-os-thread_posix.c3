// Source: https://github.com/c3lang/c3c/blob/master/lib/std/threads/os/thread_posix.c3
// Permalink: https://github.com/c3lang/c3c/blob/edc55a2/lib/std/threads/os/thread_posix.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::thread::os @if(env::POSIX);
import std::os::posix, std::time, libc;

struct NativeMutex
{
	Pthread_mutex_t mutex;
	bool initialized;
}

def NativeConditionVariable = Pthread_cond_t;
def NativeThread = Pthread_t;
def NativeOnceFlag = Pthread_once_t;

/**
 * @require !self.is_initialized() : "Mutex is already initialized"
 * @ensure self.is_initialized()
 **/
fn void! NativeMutex.init(&self, MutexType type)
{
	Pthread_mutexattr_t attr;
	if (posix::pthread_mutexattr_init(&attr)) return ThreadFault.INIT_FAILED?;
	defer posix::pthread_mutexattr_destroy(&attr);
	if (type & thread::MUTEX_RECURSIVE)
	{
		if (posix::pthread_mutexattr_settype(&attr, posix::PTHREAD_MUTEX_RECURSIVE)) return ThreadFault.INIT_FAILED?;
	}
	if (posix::pthread_mutex_init(&self.mutex, &attr)) return ThreadFault.INIT_FAILED?;
	self.initialized = true;
}

fn bool NativeMutex.is_initialized(&self)
{
	return self.initialized;
}

/**
 * @require self.is_initialized() : "Mutex was not initialized"
 * @ensure !self.is_initialized()
 **/
fn void! NativeMutex.destroy(&self)
{
	if (posix::pthread_mutex_destroy(&self.mutex)) return ThreadFault.DESTROY_FAILED?;
	*self = {};
}

/**
 * @require self.is_initialized() : "Mutex was not initialized"
 **/
fn void! NativeMutex.lock(&self)
{
	if (posix::pthread_mutex_lock(&self.mutex)) return ThreadFault.LOCK_FAILED?;
}

/**
 * @require self.is_initialized() : "Mutex was not initialized"
 **/
fn void! NativeMutex.lock_timeout(&self, ulong ms)
{
	/* Try to acquire the lock and, if we fail, sleep for 5ms. */
	Errno result;
	while ((result = posix::pthread_mutex_trylock(&self.mutex)) == errno::EBUSY)
	{
		if (!ms) break;
		ulong sleep = min(5, ms);
		if (!libc::nanosleep(&&time::ms(ms).to_timespec(), null)) return ThreadFault.LOCK_FAILED?;
		ms -= sleep;
	}
	switch (result)
	{
		case errno::OK:
			return;
		case errno::EBUSY:
		case errno::ETIMEDOUT:
			return ThreadFault.LOCK_TIMEOUT?;
		default:
			return ThreadFault.LOCK_FAILED?;
	}
}

/**
 * @require self.is_initialized() : "Mutex was not initialized"
 **/
fn bool NativeMutex.try_lock(&self)
{
	return !posix::pthread_mutex_trylock(&self.mutex);
}

/**
 * @require self.is_initialized() : "Mutex was not initialized"
 **/
fn void! NativeMutex.unlock(&self)
{
	if (posix::pthread_mutex_unlock(&self.mutex)) return ThreadFault.UNLOCK_FAILED?;
}

fn void! NativeConditionVariable.init(&cond)
{
	if (posix::pthread_cond_init(cond, null)) return ThreadFault.INIT_FAILED?;
}

fn void! NativeConditionVariable.destroy(&cond)
{
	if (posix::pthread_cond_destroy(cond)) return ThreadFault.DESTROY_FAILED?;
}

fn void! NativeConditionVariable.signal(&cond)
{
	if (posix::pthread_cond_signal(cond)) return ThreadFault.SIGNAL_FAILED?;
}

fn void! NativeConditionVariable.broadcast(&cond)
{
	if (posix::pthread_cond_broadcast(cond)) return ThreadFault.SIGNAL_FAILED?;
}

/**
 * @require mtx.is_initialized()
 **/
fn void! NativeConditionVariable.wait(&cond, NativeMutex* mtx)
{
	if (posix::pthread_cond_wait(cond, &mtx.mutex)) return ThreadFault.WAIT_FAILED?;
}

/**
 * @require mtx.is_initialized()
 **/
fn void! NativeConditionVariable.wait_timeout(&cond, NativeMutex* mtx, ulong ms)
{
	TimeSpec now;
	if (libc::timespec_get(&now, libc::TIME_UTC) != libc::TIME_UTC) return ThreadFault.WAIT_FAILED?;
	now.ns += (CLong)((ms % 1000) * 1000_000);
	now.s += (Time_t)(ms / 1000 + now.ns / 1000_000_000);
	now.ns = now.ns % 1000_000_000;
	switch (posix::pthread_cond_timedwait(cond, &mtx.mutex, &now))
	{
		case errno::ETIMEDOUT:
			return ThreadFault.WAIT_TIMEOUT?;
		case errno::OK:
			return;
		default:
			return ThreadFault.WAIT_FAILED?;
	}
}


fn void* callback(void* arg) @private
{
	PosixThreadData *data = arg;
	return (void*)(iptr)data.thread_fn(data.arg);
}

fn void! NativeThread.create(&thread, ThreadFn thread_fn, void* arg)
{

	PosixThreadData *thread_data = mem::new(PosixThreadData, { .thread_fn = thread_fn, .arg = arg });
	if (posix::pthread_create(thread, null, &callback, thread_data) != 0)
	{
		*thread = null;
		free(thread_data);
		return ThreadFault.INIT_FAILED?;
	}
}

fn void! NativeThread.detach(thread)
{
	if (posix::pthread_detach(thread)) return ThreadFault.DETACH_FAILED?;
}

fn void native_thread_exit(int result)
{
	posix::pthread_exit((void*)(iptr)result);
}

fn NativeThread native_thread_current()
{
	return (NativeThread)posix::pthread_self();
}

fn bool NativeThread.equals(thread, NativeThread other)
{
	return (bool)posix::pthread_equal(thread, other);
}

fn int! NativeThread.join(thread)
{
	void *pres;
	if (posix::pthread_join(thread, &pres)) return ThreadFault.JOIN_FAILED?;
	return (int)(iptr)pres;
}

fn void NativeOnceFlag.call_once(&flag, OnceFn func)
{
	posix::pthread_once(flag, func);
}

fn void native_thread_yield()
{
	posix::sched_yield();
}

struct PosixThreadData @private
{
	ThreadFn thread_fn;
	void* arg;
}

fn void! native_sleep_nano(NanoDuration nano)
{
	if (nano <= 0) return;
	if (libc::nanosleep(&&nano.to_timespec(), null)) return ThreadFault.INTERRUPTED?;
}

