/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2_util.h"

#ifndef GIT_WIN32
#include <sys/time.h>
#endif

#include "util.h"
#include "posix.h"
#include "date.h"

#include <ctype.h>
#include <time.h>

typedef enum {
	DATE_NORMAL = 0,
	DATE_RELATIVE,
	DATE_SHORT,
	DATE_LOCAL,
	DATE_ISO8601,
	DATE_RFC2822,
	DATE_RAW
} date_mode;

/*
 * This is like mktime, but without normalization of tm_wday and tm_yday.
 */
static git_time_t tm_to_time_t(const struct tm *tm)
{
	static const int mdays[] = {
	    0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334
	};
	int year = tm->tm_year - 70;
	int month = tm->tm_mon;
	int day = tm->tm_mday;

	if (year < 0 || year > 129) /* algo only works for 1970-2099 */
		return -1;
	if (month < 0 || month > 11) /* array bounds */
		return -1;
	if (month < 2 || (year + 2) % 4)
		day--;
	if (tm->tm_hour < 0 || tm->tm_min < 0 || tm->tm_sec < 0)
		return -1;
	return (year * 365 + (year + 1) / 4 + mdays[month] + day) * 24*60*60UL +
		tm->tm_hour * 60*60 + tm->tm_min * 60 + tm->tm_sec;
}

static const char *month_names[] = {
	"January", "February", "March", "April", "May", "June",
	"July", "August", "September", "October", "November", "December"
};

static const char *weekday_names[] = {
	"Sundays", "Mondays", "Tuesdays", "Wednesdays", "Thursdays", "Fridays", "Saturdays"
};



/*
 * Check these. And note how it doesn't do the summer-time conversion.
 *
 * In my world, it's always summer, and things are probably a bit off
 * in other ways too.
 */
static const struct {
	const char *name;
	int offset;
	int dst;
} timezone_names[] = {
	{ "IDLW", -12, 0, },	/* International Date Line West */
	{ "NT",   -11, 0, },	/* Nome */
	{ "CAT",  -10, 0, },	/* Central Alaska */
	{ "HST",  -10, 0, },	/* Hawaii Standard */
	{ "HDT",  -10, 1, },	/* Hawaii Daylight */
	{ "YST",   -9, 0, },	/* Yukon Standard */
	{ "YDT",   -9, 1, },	/* Yukon Daylight */
	{ "PST",   -8, 0, },	/* Pacific Standard */
	{ "PDT",   -8, 1, },	/* Pacific Daylight */
	{ "MST",   -7, 0, },	/* Mountain Standard */
	{ "MDT",   -7, 1, },	/* Mountain Daylight */
	{ "CST",   -6, 0, },	/* Central Standard */
	{ "CDT",   -6, 1, },	/* Central Daylight */
	{ "EST",   -5, 0, },	/* Eastern Standard */
	{ "EDT",   -5, 1, },	/* Eastern Daylight */
	{ "AST",   -3, 0, },	/* Atlantic Standard */
	{ "ADT",   -3, 1, },	/* Atlantic Daylight */
	{ "WAT",   -1, 0, },	/* West Africa */

	{ "GMT",    0, 0, },	/* Greenwich Mean */
	{ "UTC",    0, 0, },	/* Universal (Coordinated) */
	{ "Z",      0, 0, },    /* Zulu, alias for UTC */

	{ "WET",    0, 0, },	/* Western European */
	{ "BST",    0, 1, },	/* British Summer */
	{ "CET",   +1, 0, },	/* Central European */
	{ "MET",   +1, 0, },	/* Middle European */
	{ "MEWT",  +1, 0, },	/* Middle European Winter */
	{ "MEST",  +1, 1, },	/* Middle European Summer */
	{ "CEST",  +1, 1, },	/* Central European Summer */
	{ "MESZ",  +1, 1, },	/* Middle European Summer */
	{ "FWT",   +1, 0, },	/* French Winter */
	{ "FST",   +1, 1, },	/* French Summer */
	{ "EET",   +2, 0, },	/* Eastern Europe */
	{ "EEST",  +2, 1, },	/* Eastern European Daylight */
	{ "WAST",  +7, 0, },	/* West Australian Standard */
	{ "WADT",  +7, 1, },	/* West Australian Daylight */
	{ "CCT",   +8, 0, },	/* China Coast */
	{ "JST",   +9, 0, },	/* Japan Standard */
	{ "EAST", +10, 0, },	/* Eastern Australian Standard */
	{ "EADT", +10, 1, },	/* Eastern Australian Daylight */
	{ "GST",  +10, 0, },	/* Guam Standard */
	{ "NZT",  +12, 0, },	/* New Zealand */
	{ "NZST", +12, 0, },	/* New Zealand Standard */
	{ "NZDT", +12, 1, },	/* New Zealand Daylight */
	{ "IDLE", +12, 0, },	/* International Date Line East */
};

static size_t match_string(const char *date, const char *str)
{
	size_t i = 0;

	for (i = 0; *date; date++, str++, i++) {
		if (*date == *str)
			continue;
		if (git__toupper(*date) == git__toupper(*str))
			continue;
		if (!git__isalnum(*date))
			break;
		return 0;
	}
	return i;
}

static int skip_alpha(const char *date)
{
	int i = 0;
	do {
		i++;
	} while (git__isalpha(date[i]));
	return i;
}

/*
* Parse month, weekday, or timezone name
*/
static size_t match_alpha(const char *date, struct tm *tm, int *offset)
{
	unsigned int i;

	for (i = 0; i < 12; i++) {
		size_t match = match_string(date, month_names[i]);
		if (match >= 3) {
			tm->tm_mon = i;
			return match;
		}
	}

	for (i = 0; i < 7; i++) {
		size_t match = match_string(date, weekday_names[i]);
		if (match >= 3) {
			tm->tm_wday = i;
			return match;
		}
	}

	for (i = 0; i < ARRAY_SIZE(timezone_names); i++) {
		size_t match = match_string(date, timezone_names[i].name);
		if (match >= 3 || match == strlen(timezone_names[i].name)) {
			int off = timezone_names[i].offset;

			/* This is bogus, but we like summer */
			off += timezone_names[i].dst;

			/* Only use the tz name offset if we don't have anything better */
			if (*offset == -1)
				*offset = 60*off;

			return match;
		}
	}

	if (match_string(date, "PM") == 2) {
		tm->tm_hour = (tm->tm_hour % 12) + 12;
		return 2;
	}

	if (match_string(date, "AM") == 2) {
		tm->tm_hour = (tm->tm_hour % 12) + 0;
		return 2;
	}

	/* BAD */
	return skip_alpha(date);
}

static int is_date(int year, int month, int day, struct tm *now_tm, time_t now, struct tm *tm)
{
	if (month > 0 && month < 13 && day > 0 && day < 32) {
		struct tm check = *tm;
		struct tm *r = (now_tm ? &check : tm);
		git_time_t specified;

		r->tm_mon = month - 1;
		r->tm_mday = day;
		if (year == -1) {
			if (!now_tm)
				return 1;
			r->tm_year = now_tm->tm_year;
		}
		else if (year >= 1970 && year < 2100)
			r->tm_year = year - 1900;
		else if (year > 70 && year < 100)
			r->tm_year = year;
		else if (year < 38)
			r->tm_year = year + 100;
		else
			return 0;
		if (!now_tm)
			return 1;

		specified = tm_to_time_t(r);

		/* Be it commit time or author time, it does not make
		 * sense to specify timestamp way into the future.  Make
		 * sure it is not later than ten days from now...
		 */
		if (now + 10*24*3600 < specified)
			return 0;
		tm->tm_mon = r->tm_mon;
		tm->tm_mday = r->tm_mday;
		if (year != -1)
			tm->tm_year = r->tm_year;
		return 1;
	}
	return 0;
}

static size_t match_multi_number(unsigned long num, char c, const char *date, char *end, struct tm *tm)
{
	time_t now;
	struct tm now_tm;
	struct tm *refuse_future;
	long num2, num3;

	num2 = strtol(end+1, &end, 10);
	num3 = -1;
	if (*end == c && git__isdigit(end[1]))
		num3 = strtol(end+1, &end, 10);

	/* Time? Date? */
	switch (c) {
	case ':':
		if (num3 < 0)
			num3 = 0;
		if (num < 25 && num2 >= 0 && num2 < 60 && num3 >= 0 && num3 <= 60) {
			tm->tm_hour = num;
			tm->tm_min = num2;
			tm->tm_sec = num3;
			break;
		}
		return 0;

	case '-':
	case '/':
	case '.':
		now = time(NULL);
		refuse_future = NULL;
		if (p_gmtime_r(&now, &now_tm))
			refuse_future = &now_tm;

		if (num > 70) {
			/* yyyy-mm-dd? */
			if (is_date(num, num2, num3, refuse_future, now, tm))
				break;
			/* yyyy-dd-mm? */
			if (is_date(num, num3, num2, refuse_future, now, tm))
				break;
		}
		/* Our eastern European friends say dd.mm.yy[yy]
		 * is the norm there, so giving precedence to
		 * mm/dd/yy[yy] form only when separator is not '.'
		 */
		if (c != '.' &&
		    is_date(num3, num, num2, refuse_future, now, tm))
			break;
		/* European dd.mm.yy[yy] or funny US dd/mm/yy[yy] */
		if (is_date(num3, num2, num, refuse_future, now, tm))
			break;
		/* Funny European mm.dd.yy */
		if (c == '.' &&
		    is_date(num3, num, num2, refuse_future, now, tm))
			break;
		return 0;
	}
	return end - date;
}

/*
 * Have we filled in any part of the time/date yet?
 * We just do a binary 'and' to see if the sign bit
 * is set in all the values.
 */
static int nodate(struct tm *tm)
{
	return (tm->tm_year &
		tm->tm_mon &
		tm->tm_mday &
		tm->tm_hour &
		tm->tm_min &
		tm->tm_sec) < 0;
}

/*
 * We've seen a digit. Time? Year? Date?
 */
static size_t match_digit(const char *date, struct tm *tm, int *offset, int *tm_gmt)
{
	size_t n;
	char *end;
	unsigned long num;

	num = strtoul(date, &end, 10);

	/*
	 * Seconds since 1970? We trigger on that for any numbers with
	 * more than 8 digits. This is because we don't want to rule out
	 * numbers like 20070606 as a YYYYMMDD date.
	 */
	if (num >= 100000000 && nodate(tm)) {
		time_t time = num;
		if (p_gmtime_r(&time, tm)) {
			*tm_gmt = 1;
			return end - date;
		}
	}

	/*
	 * Check for special formats: num[-.:/]num[same]num
	 */
	switch (*end) {
	case ':':
	case '.':
	case '/':
	case '-':
		if (git__isdigit(end[1])) {
			size_t match = match_multi_number(num, *end, date, end, tm);
			if (match)
				return match;
		}
	}

	/*
	 * None of the special formats? Try to guess what
	 * the number meant. We use the number of digits
	 * to make a more educated guess..
	 */
	n = 0;
	do {
		n++;
	} while (git__isdigit(date[n]));

	/* Four-digit year or a timezone? */
	if (n == 4) {
		if (num <= 1400 && *offset == -1) {
			unsigned int minutes = num % 100;
			unsigned int hours = num / 100;
			*offset = hours*60 + minutes;
		} else if (num > 1900 && num < 2100)
			tm->tm_year = num - 1900;
		return n;
	}

	/*
	 * Ignore lots of numerals. We took care of 4-digit years above.
	 * Days or months must be one or two digits.
	 */
	if (n > 2)
		return n;

	/*
	 * NOTE! We will give precedence to day-of-month over month or
	 * year numbers in the 1-12 range. So 05 is always "mday 5",
	 * unless we already have a mday..
	 *
	 * IOW, 01 Apr 05 parses as "April 1st, 2005".
	 */
	if (num > 0 && num < 32 && tm->tm_mday < 0) {
		tm->tm_mday = num;
		return n;
	}

	/* Two-digit year? */
	if (n == 2 && tm->tm_year < 0) {
		if (num < 10 && tm->tm_mday >= 0) {
			tm->tm_year = num + 100;
			return n;
		}
		if (num >= 70) {
			tm->tm_year = num;
			return n;
		}
	}

	if (num > 0 && num < 13 && tm->tm_mon < 0)
		tm->tm_mon = num-1;

	return n;
}

static size_t match_tz(const char *date, int *offp)
{
	char *end;
	int hour = strtoul(date + 1, &end, 10);
	size_t n = end - (date + 1);
	int min = 0;

	if (n == 4) {
		/* hhmm */
		min = hour % 100;
		hour = hour / 100;
	} else if (n != 2) {
		min = 99; /* random stuff */
	} else if (*end == ':') {
		/* hh:mm? */
		min = strtoul(end + 1, &end, 10);
		if (end - (date + 1) != 5)
			min = 99; /* random stuff */
	} /* otherwise we parsed "hh" */

	/*
	 * Don't accept any random stuff. Even though some places have
	 * offset larger than 12 hours (e.g. Pacific/Kiritimati is at
	 * UTC+14), there is something wrong if hour part is much
	 * larger than that. We might also want to check that the
	 * minutes are divisible by 15 or something too. (Offset of
	 * Kathmandu, Nepal is UTC+5:45)
	 */
	if (min < 60 && hour < 24) {
		int offset = hour * 60 + min;
		if (*date == '-')
			offset = -offset;
		*offp = offset;
	}
	return end - date;
}

/*
 * Parse a string like "0 +0000" as ancient timestamp near epoch, but
 * only when it appears not as part of any other string.
 */
static int match_object_header_date(const char *date, git_time_t *timestamp, int *offset)
{
	char *end;
	unsigned long stamp;
	int ofs;

	if (*date < '0' || '9' <= *date)
		return -1;
	stamp = strtoul(date, &end, 10);
	if (*end != ' ' || stamp == ULONG_MAX || (end[1] != '+' && end[1] != '-'))
		return -1;
	date = end + 2;
	ofs = strtol(date, &end, 10);
	if ((*end != '\0' && (*end != '\n')) || end != date + 4)
		return -1;
	ofs = (ofs / 100) * 60 + (ofs % 100);
	if (date[-1] == '-')
		ofs = -ofs;
	*timestamp = stamp;
	*offset = ofs;
	return 0;
}

/* Gr. strptime is crap for this; it doesn't have a way to require RFC2822
   (i.e. English) day/month names, and it doesn't work correctly with %z. */
static int parse_date_basic(const char *date, git_time_t *timestamp, int *offset)
{
	struct tm tm;
	int tm_gmt;
	git_time_t dummy_timestamp;
	int dummy_offset;

	if (!timestamp)
		timestamp = &dummy_timestamp;
	if (!offset)
		offset = &dummy_offset;

	memset(&tm, 0, sizeof(tm));
	tm.tm_year = -1;
	tm.tm_mon = -1;
	tm.tm_mday = -1;
	tm.tm_isdst = -1;
	tm.tm_hour = -1;
	tm.tm_min = -1;
	tm.tm_sec = -1;
	*offset = -1;
	tm_gmt = 0;

	if (*date == '@' &&
	    !match_object_header_date(date + 1, timestamp, offset))
		return 0; /* success */
	for (;;) {
		size_t match = 0;
		unsigned char c = *date;

		/* Stop at end of string or newline */
		if (!c || c == '\n')
			break;

		if (git__isalpha(c))
			match = match_alpha(date, &tm, offset);
		else if (git__isdigit(c))
			match = match_digit(date, &tm, offset, &tm_gmt);
		else if ((c == '-' || c == '+') && git__isdigit(date[1]))
			match = match_tz(date, offset);

		if (!match) {
			/* BAD */
			match = 1;
		}

		date += match;
	}

	/* mktime uses local timezone */
	*timestamp = tm_to_time_t(&tm);
	if (*offset == -1)
		*offset = (int)((time_t)*timestamp - mktime(&tm)) / 60;

	if (*timestamp == (git_time_t)-1)
		return -1;

	if (!tm_gmt)
		*timestamp -= *offset * 60;
	return 0; /* success */
}


/*
 * Relative time update (eg "2 days ago").  If we haven't set the time
 * yet, we need to set it from current time.
 */
static git_time_t update_tm(struct tm *tm, struct tm *now, unsigned long sec)
{
	time_t n;

	if (tm->tm_mday < 0)
		tm->tm_mday = now->tm_mday;
	if (tm->tm_mon < 0)
		tm->tm_mon = now->tm_mon;
	if (tm->tm_year < 0) {
		tm->tm_year = now->tm_year;
		if (tm->tm_mon > now->tm_mon)
			tm->tm_year--;
	}

	n = mktime(tm) - sec;
	p_localtime_r(&n, tm);
	return n;
}

static void date_now(struct tm *tm, struct tm *now, int *num)
{
   GIT_UNUSED(num);
	update_tm(tm, now, 0);
}

static void date_yesterday(struct tm *tm, struct tm *now, int *num)
{
   GIT_UNUSED(num);
	update_tm(tm, now, 24*60*60);
}

static void date_time(struct tm *tm, struct tm *now, int hour)
{
	if (tm->tm_hour < hour)
		date_yesterday(tm, now, NULL);
	tm->tm_hour = hour;
	tm->tm_min = 0;
	tm->tm_sec = 0;
}

static void date_midnight(struct tm *tm, struct tm *now, int *num)
{
   GIT_UNUSED(num);
	date_time(tm, now, 0);
}

static void date_noon(struct tm *tm, struct tm *now, int *num)
{
   GIT_UNUSED(num);
	date_time(tm, now, 12);
}

static void date_tea(struct tm *tm, struct tm *now, int *num)
{
   GIT_UNUSED(num);
	date_time(tm, now, 17);
}

static void date_pm(struct tm *tm, struct tm *now, int *num)
{
	int hour, n = *num;
	*num = 0;
   GIT_UNUSED(now);

	hour = tm->tm_hour;
	if (n) {
		hour = n;
		tm->tm_min = 0;
		tm->tm_sec = 0;
	}
	tm->tm_hour = (hour % 12) + 12;
}

static void date_am(struct tm *tm, struct tm *now, int *num)
{
	int hour, n = *num;
	*num = 0;
   GIT_UNUSED(now);

	hour = tm->tm_hour;
	if (n) {
		hour = n;
		tm->tm_min = 0;
		tm->tm_sec = 0;
	}
	tm->tm_hour = (hour % 12);
}

static void date_never(struct tm *tm, struct tm *now, int *num)
{
	time_t n = 0;
   GIT_UNUSED(now);
   GIT_UNUSED(num);
	p_localtime_r(&n, tm);
}

static const struct special {
	const char *name;
	void (*fn)(struct tm *, struct tm *, int *);
} special[] = {
	{ "yesterday", date_yesterday },
	{ "noon", date_noon },
	{ "midnight", date_midnight },
	{ "tea", date_tea },
	{ "PM", date_pm },
	{ "AM", date_am },
	{ "never", date_never },
	{ "now", date_now },
	{ NULL }
};

static const char *number_name[] = {
	"zero", "one", "two", "three", "four",
	"five", "six", "seven", "eight", "nine", "ten",
};

static const struct typelen {
	const char *type;
	int length;
} typelen[] = {
	{ "seconds", 1 },
	{ "minutes", 60 },
	{ "hours", 60*60 },
	{ "days", 24*60*60 },
	{ "weeks", 7*24*60*60 },
	{ NULL }
};

static const char *approxidate_alpha(const char *date, struct tm *tm, struct tm *now, int *num, int *touched)
{
	const struct typelen *tl;
	const struct special *s;
	const char *end = date;
	int i;

	while (git__isalpha(*++end))
		/* scan to non-alpha */;

	for (i = 0; i < 12; i++) {
		size_t match = match_string(date, month_names[i]);
		if (match >= 3) {
			tm->tm_mon = i;
			*touched = 1;
			return end;
		}
	}

	for (s = special; s->name; s++) {
		size_t len = strlen(s->name);
		if (match_string(date, s->name) == len) {
			s->fn(tm, now, num);
			*touched = 1;
			return end;
		}
	}

	if (!*num) {
		for (i = 1; i < 11; i++) {
			size_t len = strlen(number_name[i]);
			if (match_string(date, number_name[i]) == len) {
				*num = i;
				*touched = 1;
				return end;
			}
		}
		if (match_string(date, "last") == 4) {
			*num = 1;
			*touched = 1;
		}
		return end;
	}

	tl = typelen;
	while (tl->type) {
		size_t len = strlen(tl->type);
		if (match_string(date, tl->type) >= len-1) {
			update_tm(tm, now, tl->length * (unsigned long)*num);
			*num = 0;
			*touched = 1;
			return end;
		}
		tl++;
	}

	for (i = 0; i < 7; i++) {
		size_t match = match_string(date, weekday_names[i]);
		if (match >= 3) {
			int diff, n = *num -1;
			*num = 0;

			diff = tm->tm_wday - i;
			if (diff <= 0)
				n++;
			diff += 7*n;

			update_tm(tm, now, diff * 24 * 60 * 60);
			*touched = 1;
			return end;
		}
	}

	if (match_string(date, "months") >= 5) {
		int n;
		update_tm(tm, now, 0); /* fill in date fields if needed */
		n = tm->tm_mon - *num;
		*num = 0;
		while (n < 0) {
			n += 12;
			tm->tm_year--;
		}
		tm->tm_mon = n;
		*touched = 1;
		return end;
	}

	if (match_string(date, "years") >= 4) {
		update_tm(tm, now, 0); /* fill in date fields if needed */
		tm->tm_year -= *num;
		*num = 0;
		*touched = 1;
		return end;
	}

	return end;
}

static const char *approxidate_digit(const char *date, struct tm *tm, int *num)
{
	char *end;
	unsigned long number = strtoul(date, &end, 10);

	switch (*end) {
	case ':':
	case '.':
	case '/':
	case '-':
		if (git__isdigit(end[1])) {
			size_t match = match_multi_number(number, *end, date, end, tm);
			if (match)
				return date + match;
		}
	}

	/* Accept zero-padding only for small numbers ("Dec 02", never "Dec 0002") */
	if (date[0] != '0' || end - date <= 2)
		*num = number;
	return end;
}

/*
 * Do we have a pending number at the end, or when
 * we see a new one? Let's assume it's a month day,
 * as in "Dec 6, 1992"
 */
static void pending_number(struct tm *tm, int *num)
{
	int number = *num;

	if (number) {
		*num = 0;
		if (tm->tm_mday < 0 && number < 32)
			tm->tm_mday = number;
		else if (tm->tm_mon < 0 && number < 13)
			tm->tm_mon = number-1;
		else if (tm->tm_year < 0) {
			if (number > 1969 && number < 2100)
				tm->tm_year = number - 1900;
			else if (number > 69 && number < 100)
				tm->tm_year = number;
			else if (number < 38)
				tm->tm_year = 100 + number;
			/* We mess up for number = 00 ? */
		}
	}
}

static git_time_t approxidate_str(const char *date,
	time_t time_sec,
	int *error_ret)
{
	int number = 0;
	int touched = 0;
	struct tm tm = {0}, now;

	p_localtime_r(&time_sec, &tm);
	now = tm;

	tm.tm_year = -1;
	tm.tm_mon = -1;
	tm.tm_mday = -1;

	for (;;) {
		unsigned char c = *date;
		if (!c)
			break;
		date++;
		if (git__isdigit(c)) {
			pending_number(&tm, &number);
			date = approxidate_digit(date-1, &tm, &number);
			touched = 1;
			continue;
		}
		if (git__isalpha(c))
			date = approxidate_alpha(date-1, &tm, &now, &number, &touched);
	}
	pending_number(&tm, &number);
	if (!touched)
		*error_ret = -1;
	return update_tm(&tm, &now, 0);
}

int git_date_offset_parse(git_time_t *out, int *out_offset, const char *date)
{
	time_t time_sec;
	git_time_t timestamp;
	int offset, error_ret=0;

	if (!parse_date_basic(date, &timestamp, &offset)) {
		*out = timestamp;
		*out_offset = offset;
		return 0;
	}

	if (time(&time_sec) == -1)
		return -1;

	*out = approxidate_str(date, time_sec, &error_ret);
	return error_ret;
}

int git_date_parse(git_time_t *out, const char *date)
{
	int offset;

	return git_date_offset_parse(out, &offset, date);
}

int git_date_rfc2822_fmt(git_str *out, git_time_t time, int offset)
{
	time_t t;
	struct tm gmt;

	GIT_ASSERT_ARG(out);

	t = (time_t) (time + offset * 60);

	if (p_gmtime_r(&t, &gmt) == NULL)
		return -1;

	return git_str_printf(out, "%.3s, %u %.3s %.4u %02u:%02u:%02u %+03d%02d",
		weekday_names[gmt.tm_wday],
		gmt.tm_mday,
		month_names[gmt.tm_mon],
		gmt.tm_year + 1900,
		gmt.tm_hour, gmt.tm_min, gmt.tm_sec,
		offset / 60, offset % 60);
}

