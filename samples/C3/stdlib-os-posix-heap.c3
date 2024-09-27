// Source: https://github.com/c3lang/c3c/blob/master/lib/std/os/posix/heap.c3
// Permalink: https://github.com/c3lang/c3c/blob/7ea3d23/lib/std/os/posix/heap.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::os::posix @if(env::POSIX);

extern fn CInt posix_memalign(void **memptr, usz alignment, usz size);