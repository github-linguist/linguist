// Source: https://github.com/c3lang/c3c/blob/master/lib/std/time/os/time_posix.c3
// Permalink: https://github.com/c3lang/c3c/blob/68f6cb1/lib/std/time/os/time_posix.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::time::os @if(env::POSIX);
import libc;

extern fn void clock_gettime(int type, TimeSpec *time);

fn Time native_timestamp()
{
	TimeSpec ts;
	clock_gettime(CLOCK_REALTIME, &ts);
	return (Time)(ts.s * 1_000_000i64 + ts.ns / 1_000i64);
}

fn Clock native_clock() @if(!env::DARWIN)
{
	TimeSpec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return (Clock)((ulong)ts.s * 1_000_000_000u64 + (ulong)ts.ns);
}

module std::time::os @if(env::OPENBSD);
const CLOCK_REALTIME = 0;
const CLOCK_PROCESS_CPUTIME_ID = 2;
const CLOCK_MONOTONIC = 3;
const CLOCK_THREAD_CPUTIME_ID = 4;
const CLOCK_UPTIME = 5;
const CLOCK_BOOTTIME = 6;

module std::time::os @if(env::FREEBSD);
const CLOCK_REALTIME = 0;
const CLOCK_VIRTUAL = 1;
const CLOCK_PROF = 2;
const CLOCK_MONOTONIC = 4;
const CLOCK_UPTIME = 5;
const CLOCK_UPTIME_PRECISE = 7;
const CLOCK_UPTIME_FAST = 8;
const CLOCK_REALTIME_PRECISE = 9;
const CLOCK_REALTIME_FAST = 10;
const CLOCK_MONOTONIC_PRECISE = 11;
const CLOCK_MONOTONIC_FAST = 12;
const CLOCK_SECOND = 13;
const CLOCK_THREAD_CPUTIME_ID = 14;
const CLOCK_PROCESS_CPUTIME_ID = 15;
const CLOCK_BOOTTIME = CLOCK_UPTIME;
const CLOCK_REALTIME_COARSE = CLOCK_REALTIME_FAST;
const CLOCK_MONOTONIC_COARSE = CLOCK_MONOTONIC_FAST;

module std::time::os @if(env::NETBSD);
const CLOCK_REALTIME = 0;
const CLOCK_VIRTUAL = 1;
const CLOCK_PROF = 2;
const CLOCK_MONOTONIC = 3;
const CLOCK_THREAD_CPUTIME_ID = 0x20000000;
const CLOCK_PROCESS_CPUTIME_ID = 0x40000000;

module std::time::os @if(env::WASI);
// Not implemented
const CLOCK_REALTIME = 0;
const CLOCK_MONOTONIC = 0;

module std::time::os @if(env::DARWIN);
const CLOCK_REALTIME = 0;
const CLOCK_MONOTONIC = 6;
const CLOCK_MONOTONIC_RAW = 4;
const CLOCK_MONOTONIC_RAW_APPROX = 5;
const CLOCK_UPTIME_RAW = 8;
const CLOCK_UPTIME_RAW_APPROX = 9;
const CLOCK_PROCESS_CPUTIME_ID = 12;
const CLOCK_THREAD_CPUTIME_ID = 16;

module std::time::os @if(env::LINUX);
const CLOCK_REALTIME = 0;
const CLOCK_MONOTONIC = 1;
const CLOCK_PROCESS_CPUTIME_ID = 2;
const CLOCK_THREAD_CPUTIME_ID = 3;
const CLOCK_MONOTONIC_RAW = 4;
const CLOCK_REALTIME_COARSE = 5;
const CLOCK_MONOTONIC_COARSE = 6;
const CLOCK_BOOTTIME = 7;
const CLOCK_REALTIME_ALARM = 8;
const CLOCK_BOOTTIME_ALARM = 9;
const CLOCK_TAI = 11;

