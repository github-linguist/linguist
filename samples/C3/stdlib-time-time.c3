// Source: https://github.com/c3lang/c3c/blob/master/lib/std/time/time.c3
// Permalink: https://github.com/c3lang/c3c/blob/edc55a2/lib/std/time/time.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::time;
import std::io, std::time::os;

distinct Time = long;
distinct Duration = long;
distinct Clock = ulong;
distinct NanoDuration (Printable) = long;

const Duration US = 1;
const Duration MS = 1_000;
const Duration SEC = 1_000_000;
const Duration MIN = 60 * SEC;
const Duration HOUR = 60 * MIN;
const Duration DAY = 24 * HOUR;
const Duration WEEK = 7 * DAY;
const Duration MONTH = 30 * WEEK;
const Duration YEAR = 36525 * DAY / 100;

fn Duration us(long l) @inline => (Duration)l * US;
fn Duration ms(long l) @inline => (Duration)l * MS;
fn Duration sec(long l) @inline => (Duration)l * SEC;
fn Duration min(long l) @inline => (Duration)l * MIN;
fn Duration hour(long l) @inline => (Duration)l * HOUR;
fn Duration from_float(double s) @inline => (Duration)(s * (double)SEC);

struct DateTime
{
	int usec;
	char sec;
	char min;
	char hour;
	char day;
	Month month;
	Weekday weekday;
	int year;
	ushort year_day;
	Time time;
}

struct TzDateTime
{
	inline DateTime date_time;
	int gmt_offset;
}

enum Weekday : char
{
	MONDAY,
	TUESDAY,
	WEDNESDAY,
	THURSDAY,
	FRIDAY,
	SATURDAY,
	SUNDAY,
}

enum Month : char
{
	JANUARY,
	FEBRUARY,
	MARCH,
	APRIL,
	MAY,
	JUNE,
	JULY,
	AUGUST,
	SEPTEMBER,
	OCTOBER,
	NOVEMBER,
	DECEMBER
}


fn Time now()
{
	$if $defined(native_timestamp):
		return os::native_timestamp();
	$else
		return 0;
	$endif
}

fn Time Time.add_seconds(time, long seconds) => time + (Time)(seconds * (long)SEC);
fn Time Time.add_minutes(time, long minutes) => time + (Time)(minutes * (long)MIN);
fn Time Time.add_hours(time, long hours) => time + (Time)(hours * (long)HOUR);
fn Time Time.add_days(time, long days) => time + (Time)(days * (long)DAY);
fn Time Time.add_weeks(time, long weeks) => time + (Time)(weeks * (long)WEEK);
fn Time Time.add_duration(time, Duration duration) => time + (Time)duration;
fn double Time.to_seconds(time) => (long)time / (double)SEC;
fn Duration Time.diff_us(time, Time other) => (Duration)(time - other);
fn double Time.diff_sec(time, Time other) => (long)time.diff_us(other) / (double)SEC;
fn double Time.diff_min(time, Time other) => (long)time.diff_us(other) / (double)MIN;
fn double Time.diff_hour(time, Time other) => (long)time.diff_us(other) / (double)HOUR;
fn double Time.diff_days(time, Time other) => (long)time.diff_us(other) / (double)DAY;
fn double Time.diff_weeks(time, Time other) => (long)time.diff_us(other) / (double)WEEK;

fn double NanoDuration.to_sec(nd) => (double)nd / 1_000_000_000.0;
fn long NanoDuration.to_ms(nd) => (long)nd / 1_000_000;
fn Duration NanoDuration.to_duration(nd) => (Duration)nd / 1_000;
fn NanoDuration Duration.to_nano(td) => (NanoDuration)td * 1_000;
fn long Duration.to_ms(td) => (long)td / 1_000;

fn usz! NanoDuration.to_format(&self, Formatter* formatter) @dynamic
{
	NanoDuration nd = *self;
	if (nd == 0)
	{
		return formatter.printf("0s")!;
	}

	bool neg = nd < 0;
	if (neg) nd = -nd;

	DString str = dstring::temp_with_capacity(64);
	if (nd < 1_000_000_000)
	{
		// Less than 1s: print milliseconds, microseconds and nanoseconds.
		NanoDuration ms = nd / 1_000_000;
		if (ms > 0)
		{
			str.appendf("%dms", ms);
			nd -= ms * 1_000_000;
		}
		NanoDuration us = nd / 1000;
		if (us > 0)
		{
			str.appendf("%dµs", us);
			nd -= us * 1000;
		}
		if (nd > 0)
		{
			str.appendf("%dns", nd);
		}
	}
	else
	{
		// More than 1s: print hours, minutes and seconds.
		NanoDuration ms = (nd - nd / 1_000_000_000 * 1_000_000_000) / 1_000_000;

		nd /= 1_000_000_000;
		NanoDuration hour = nd / 3600;
		if (hour > 0)
		{
			str.appendf("%dh", hour);
			nd -= hour * 3600;
		}
		NanoDuration min = nd / 60;
		if (min > 0)
		{
			str.appendf("%dm", min);
			nd -= min * 60;
		}
		NanoDuration sec = nd;
		if (ms > 0)
		{
			// Ignore trailing zeroes.
			while (ms / 10 * 10 == ms) ms /= 10;
			str.appendf("%d.%ds", sec, ms);
		}
		else
		{
			str.appendf("%ds", sec);
		}
	}
	return formatter.printf(str.str_view())!;
}