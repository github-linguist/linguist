// Source: https://github.com/c3lang/c3c/blob/master/lib/std/time/os/time_darwin.c3
// Permalink: https://github.com/c3lang/c3c/blob/68f6cb1/lib/std/time/os/time_darwin.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::time::os @if(env::DARWIN);

struct Darwin_mach_timebase_info
{
	uint numer;
	uint denom;
}

def Darwin_mach_timebase_info_t = Darwin_mach_timebase_info;
def Darwin_mach_timebase_info_data_t = Darwin_mach_timebase_info;

extern fn void mach_timebase_info(Darwin_mach_timebase_info_data_t* timebase);
extern fn ulong mach_absolute_time();

fn Clock native_clock()
{
	static Darwin_mach_timebase_info_data_t timebase;
	if (!timebase.denom)
	{
		mach_timebase_info(&timebase);
	}
	return (Clock)(mach_absolute_time() * timebase.numer / timebase.denom);
}
