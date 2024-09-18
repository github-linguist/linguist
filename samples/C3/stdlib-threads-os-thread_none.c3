// Source: https://github.com/c3lang/c3c/blob/master/lib/std/threads/os/thread_none.c3
// Permalink: https://github.com/c3lang/c3c/blob/99cfaa1/lib/std/threads/os/thread_none.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::thread::os @if (!env::POSIX && !env::WIN32);

distinct NativeMutex = int;
distinct NativeConditionVariable = int;
distinct NativeOnceFlag = int;
distinct NativeThread = int;