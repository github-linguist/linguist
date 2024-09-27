// Source: https://github.com/c3lang/c3c/blob/master/lib/std/time/clock.c3
// Permalink: https://github.com/c3lang/c3c/blob/1f052da/lib/std/time/clock.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::time::clock;
import std::time::os;

fn Clock now()
{
	$if $defined(os::native_clock):
		return os::native_clock();
	$else
		unreachable("Clock unsupported");
	$endif
}

fn NanoDuration Clock.mark(&self)
{
	Clock mark = now();
	NanoDuration diff = (NanoDuration)(mark - *self);
	*self = mark;
	return diff;
}

fn NanoDuration Clock.to_now(self)
{
	return (NanoDuration)(now() - self);
}