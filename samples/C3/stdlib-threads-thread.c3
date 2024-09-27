// Source: https://github.com/c3lang/c3c/blob/master/lib/std/threads/thread.c3
// Permalink: https://github.com/c3lang/c3c/blob/27f2d20/lib/std/threads/thread.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::thread;
import std::thread::os;
import std::time;

distinct MutexType = int;

const MutexType MUTEX_PLAIN = 0;
const MutexType MUTEX_TIMED = 1;
const MutexType MUTEX_RECURSIVE = 2;

distinct Mutex = NativeMutex;
distinct TimedMutex = inline Mutex;
distinct RecursiveMutex = inline Mutex;
distinct TimedRecursiveMutex = inline Mutex;
distinct ConditionVariable = NativeConditionVariable;
distinct Thread = NativeThread;
distinct OnceFlag = NativeOnceFlag;
def OnceFn = fn void();

def ThreadFn = fn int(void* arg);

fault ThreadFault
{
	INIT_FAILED,
	DESTROY_FAILED,
	LOCK_FAILED,
	LOCK_TIMEOUT,
	UNLOCK_FAILED,
	SIGNAL_FAILED,
	WAIT_FAILED,
	WAIT_TIMEOUT,
	DETACH_FAILED,
	JOIN_FAILED,
	INTERRUPTED,
}

macro void! Mutex.init(&mutex) => NativeMutex.init((NativeMutex*)mutex, MUTEX_PLAIN);
macro void! TimedMutex.init(&mutex) => NativeMutex.init((NativeMutex*)mutex, MUTEX_TIMED);
macro void! RecursiveMutex.init(&mutex) => NativeMutex.init((NativeMutex*)mutex, MUTEX_RECURSIVE);
macro void! TimedRecursiveMutex.init(&mutex) => NativeMutex.init((NativeMutex*)mutex, MUTEX_TIMED | MUTEX_RECURSIVE);
macro void! Mutex.destroy(&mutex) => NativeMutex.destroy((NativeMutex*)mutex);
macro void! Mutex.lock(&mutex) => NativeMutex.lock((NativeMutex*)mutex);
macro void! TimedMutex.lock_timeout(&mutex, ulong ms) => NativeMutex.lock_timeout((NativeMutex*)mutex, ms);
macro void! TimedRecursiveMutex.lock_timeout(&mutex, ulong ms) => NativeMutex.lock_timeout((NativeMutex*)mutex, ms);
macro bool Mutex.try_lock(&mutex) => NativeMutex.try_lock((NativeMutex*)mutex);
macro void! Mutex.unlock(&mutex) => NativeMutex.unlock((NativeMutex*)mutex);

macro void! ConditionVariable.init(&cond) => NativeConditionVariable.init((NativeConditionVariable*)cond);
macro void! ConditionVariable.destroy(&cond) => NativeConditionVariable.destroy((NativeConditionVariable*)cond);
macro void! ConditionVariable.signal(&cond) => NativeConditionVariable.signal((NativeConditionVariable*)cond);
macro void! ConditionVariable.broadcast(&cond) => NativeConditionVariable.broadcast((NativeConditionVariable*)cond);
macro void! ConditionVariable.wait(&cond, Mutex* mutex)
{
	return NativeConditionVariable.wait((NativeConditionVariable*)cond, (NativeMutex*)mutex);
}
macro void! ConditionVariable.wait_timeout(&cond, Mutex* mutex, ulong ms)
{
	return NativeConditionVariable.wait_timeout((NativeConditionVariable*)cond, (NativeMutex*)mutex, ms);
}


macro void! Thread.create(&thread, ThreadFn thread_fn, void* arg) => NativeThread.create((NativeThread*)thread, thread_fn, arg);
macro void! Thread.detach(thread) => NativeThread.detach((NativeThread)thread);
macro int! Thread.join(thread) => NativeThread.join((NativeThread)thread);
macro bool Thread.equals(thread, Thread other) => NativeThread.equals((NativeThread)thread, (NativeThread)other);

macro void OnceFlag.call(&flag, OnceFn func) => NativeOnceFlag.call_once((NativeOnceFlag*)flag, func);

macro void yield() => os::native_thread_yield();
macro Thread current() => (Thread)os::native_thread_current();
macro void exit(int result) => os::native_thread_exit(result);
macro void! sleep(Duration d) @maydiscard => os::native_sleep_nano(d.to_nano());
macro void! sleep_ms(ulong ms) @maydiscard  => sleep(time::ms(ms));
macro void! sleep_ns(NanoDuration ns) @maydiscard  => os::native_sleep_nano(ns);
