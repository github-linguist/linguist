// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/sanitizer/tsan.c3
// Permalink: https://github.com/c3lang/c3c/blob/59ed118/lib/std/core/sanitizer/tsan.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::core::sanitizer::tsan;

distinct MutexFlags = inline CUInt;

const MutexFlags MUTEX_LINKER_INIT      = 1 << 0;
const MutexFlags MUTEX_WRITE_REENTRANT  = 1 << 1;
const MutexFlags MUTEX_READ_REENTRANT   = 1 << 2;
const MutexFlags MUTEX_NOT_STATIC       = 1 << 8;
const MutexFlags MUTEX_READ_LOCK        = 1 << 3;
const MutexFlags MUTEX_TRY_LOCK         = 1 << 4;
const MutexFlags MUTEX_TRY_LOCK_FAILED  = 1 << 5;
const MutexFlags MUTEX_RECURSIVE_LOCK   = 1 << 6;
const MutexFlags MUTEX_RECURSIVE_UNLOCK = 1 << 7;
const MutexFlags MUTEX_TRY_READ_LOCK = MUTEX_READ_LOCK | MUTEX_TRY_LOCK;
const MutexFlags MUTEX_TRY_READ_LOCK_FAILED = MUTEX_TRY_READ_LOCK | MUTEX_TRY_LOCK_FAILED;

macro void mutex_create(void* addr, MutexFlags flags) { $if env::THREAD_SANITIZER: __tsan_mutex_create(addr, flags); $endif }
macro void mutex_destroy(void* addr, MutexFlags flags) { $if env::THREAD_SANITIZER: __tsan_mutex_destroy(addr, flags); $endif }
macro void mutex_pre_lock(void* addr, MutexFlags flags) { $if env::THREAD_SANITIZER: __tsan_mutex_pre_lock(addr, flags); $endif }
macro void mutex_post_lock(void* addr, MutexFlags flags, CInt recursion) { $if env::THREAD_SANITIZER: __tsan_mutex_post_lock(addr, flags, recursion); $endif }
macro CInt mutex_pre_unlock(void* addr, MutexFlags flags) { $if env::THREAD_SANITIZER: return __tsan_mutex_pre_unlock(addr, flags); $else return 0; $endif }
macro void mutex_post_unlock(void* addr, MutexFlags flags) { $if env::THREAD_SANITIZER: __tsan_mutex_post_unlock(addr, flags); $endif }
macro void mutex_pre_signal(void* addr, MutexFlags flags) { $if env::THREAD_SANITIZER: __tsan_mutex_pre_signal(addr, flags); $endif }
macro void mutex_post_signal(void* addr, MutexFlags flags) { $if env::THREAD_SANITIZER: __tsan_mutex_post_signal(addr, flags); $endif }
macro void mutex_pre_divert(void* addr, MutexFlags flags) { $if env::THREAD_SANITIZER: __tsan_mutex_pre_divert(addr, flags); $endif }
macro void mutex_post_divert(void* addr, MutexFlags flags) { $if env::THREAD_SANITIZER: __tsan_mutex_post_divert(addr, flags); $endif }

module std::core::sanitizer::tsan @if(env::THREAD_SANITIZER) @private;

extern fn void __tsan_mutex_create(void* addr, CUInt flags);
extern fn void __tsan_mutex_destroy(void* addr, CUInt flags);
extern fn void __tsan_mutex_pre_lock(void* addr, CUInt flags);
extern fn void __tsan_mutex_post_lock(void* addr, CUInt flags, CInt recursion);
extern fn CInt __tsan_mutex_pre_unlock(void* addr, CUInt flags);
extern fn void __tsan_mutex_post_unlock(void* addr, CUInt flags);
extern fn void __tsan_mutex_pre_signal(void* addr, CUInt flags);
extern fn void __tsan_mutex_post_signal(void* addr, CUInt flags);
extern fn void __tsan_mutex_pre_divert(void* addr, CUInt flags);
extern fn void __tsan_mutex_post_divert(void* addr, CUInt flags);
