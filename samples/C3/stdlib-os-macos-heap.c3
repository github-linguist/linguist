// Source: https://github.com/c3lang/c3c/blob/master/lib/std/os/macos/heap.c3
// Permalink: https://github.com/c3lang/c3c/blob/7ea3d23/lib/std/os/macos/heap.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::os::darwin @if(env::DARWIN);

extern fn usz malloc_size(void* ptr);
