use v6;
use Test;

# calendar.t: tests some calendar-related methods common to
# Date and DateTime

plan 130;

sub date($year, $month, $day) {
    Date.new(:$year, :$month, :$day)
}

sub dtim($year, $month, $day) {
    DateTime.new(:$year, :$month, :$day,
        :hour(17), :minute(33), :second(2.9))
}

# --------------------------------------------------------------------
# L<S32::Temporal/C<DateTime>/'truncated-to'>
# --------------------------------------------------------------------

is ~date(1969, 7, 20).truncated-to(month), '1969-07-01', 'Date.truncated-to(month)';
is ~dtim(1969, 7, 20).truncated-to(month), '1969-07-01T00:00:00Z', 'DateTime.truncated-to(month)';
is ~date(1969, 7, 20).truncated-to(year), '1969-01-01', 'Date.truncated-to(year)';
is ~dtim(1969, 7, 20).truncated-to(year), '1969-01-01T00:00:00Z', 'DateTime.truncated-to(year)';

is ~date(1999, 1, 18).truncated-to(week), '1999-01-18', 'Date.truncated-to(week) (no change in day)';
is ~date(1999, 1, 19).truncated-to(week), '1999-01-18', 'Date.truncated-to(week) (short jump)';
is ~date(1999, 1, 17).truncated-to(week), '1999-01-11', 'Date.truncated-to(week) (long jump)';
is ~dtim(1999, 1, 17).truncated-to(week), '1999-01-11T00:00:00Z', 'DateTime.truncated-to(week) (long jump)';
is ~date(1999, 4,  2).truncated-to(week), '1999-03-29', 'Date.truncated-to(week) (changing month)';
is ~date(1999, 1,  3).truncated-to(week), '1998-12-28', 'Date.truncated-to(week) (changing year)';
is ~dtim(1999, 1,  3).truncated-to(week), '1998-12-28T00:00:00Z', 'DateTime.truncated-to(week) (changing year)';
is ~date(2000, 3,  1).truncated-to(week), '2000-02-28', 'Date.truncated-to(week) (skipping over Feb 29)';
is ~dtim(2000, 3,  1).truncated-to(week), '2000-02-28T00:00:00Z', 'DateTime.truncated-to(week) (skipping over Feb 29)';
is ~date(1988, 3,  3).truncated-to(week), '1988-02-29', 'Date.truncated-to(week) (landing on Feb 29)';
is ~dtim(1988, 3,  3).truncated-to(week), '1988-02-29T00:00:00Z', 'DateTime.truncated-to(week) (landing on Feb 29)';

# Verify .gist
# Example taken from S32 specs documentation.
#?niecza skip 'Undeclared routine: hour'
{
    my $dt = DateTime.new('2005-02-01T15:20:35Z');
    my $truncated = $dt.truncated-to(hour);

    is $truncated.gist, "2005-02-01T15:00:00Z", "validate .gist output";
}

# --------------------------------------------------------------------
# L<S32::Temporal/Accessors/'the synonym day-of-month'>
# --------------------------------------------------------------------

is date(2003, 3, 18).day-of-month, 18, 'Date.day can be spelled as Date.day-of-month';
is dtim(2003, 3, 18).day-of-month, 18, 'DateTime.day can be spelled as DateTime.day-of-month';

# --------------------------------------------------------------------
# L<S32::Temporal/Accessors/'day-of-week method'>
# --------------------------------------------------------------------

# much of this is blatantly stolen from the Date::Simple test suite
# and redistributed under the terms of the Artistic License 2.0 with
# permission of the original authors (John Tobey, Marty Pauly).

is date(1966, 10, 15).day-of-week, 6, 'Date.day-of-week (1966-10-15)';
is dtim(1966, 10, 15).day-of-week, 6, 'DateTime.day-of-week (1966-10-15)';
is date(2401,  3,  1).day-of-week, 4, 'Date.day-of-week (2401-03-01)';
is date(2401,  2, 28).day-of-week, 3, 'Date.day-of-week (2401-02-28)';
is date(2400,  3,  1).day-of-week, 3, 'Date.day-of-week (2400-03-01)';
is date(2400,  2, 29).day-of-week, 2, 'Date.day-of-week (2400-02-29)';
is date(2400,  2, 28).day-of-week, 1, 'Date.day-of-week (2400-02-28)';
is date(2101,  3,  1).day-of-week, 2, 'Date.day-of-week (2101-03-01)';
is date(2101,  2, 28).day-of-week, 1, 'Date.day-of-week (2101-02-28)';
is date(2100,  3,  1).day-of-week, 1, 'Date.day-of-week (2100-03-01)';
is dtim(2100,  3,  1).day-of-week, 1, 'DateTime.day-of-week (2100-03-01)';
is date(2100,  2, 28).day-of-week, 7, 'Date.day-of-week (2100-02-28)';
is dtim(2100,  2, 28).day-of-week, 7, 'DateTime.day-of-week (2100-02-28)';
is date(2001,  3,  1).day-of-week, 4, 'Date.day-of-week (2001-03-01)';
is date(2001,  2, 28).day-of-week, 3, 'Date.day-of-week (2001-02-28)';
is date(2000,  3,  1).day-of-week, 3, 'Date.day-of-week (2000-03-01)';
is date(2000,  2, 29).day-of-week, 2, 'Date.day-of-week (2000-02-29)';
is date(2000,  2, 28).day-of-week, 1, 'Date.day-of-week (2000-02-28)';
is date(1901,  3,  1).day-of-week, 5, 'Date.day-of-week (1901-03-01)';
is date(1901,  2, 28).day-of-week, 4, 'Date.day-of-week (1901-02-28)';
is date(1900,  3,  1).day-of-week, 4, 'Date.day-of-week (1900-03-01)';
is date(1900,  2, 28).day-of-week, 3, 'Date.day-of-week (1900-02-28)';
is date(1801,  3,  1).day-of-week, 7, 'Date.day-of-week (1801-03-01)';
is date(1801,  2, 28).day-of-week, 6, 'Date.day-of-week (1801-02-28)';
is date(1800,  3,  1).day-of-week, 6, 'Date.day-of-week (1800-03-01)';
is dtim(1800,  3,  1).day-of-week, 6, 'DateTime.day-of-week (1800-03-01)';
is date(1800,  2, 28).day-of-week, 5, 'Date.day-of-week (1800-02-28)';
is dtim(1800,  2, 28).day-of-week, 5, 'DateTime.day-of-week (1800-02-28)';
is date(1701,  3,  1).day-of-week, 2, 'Date.day-of-week (1701-03-01)';
is date(1701,  2, 28).day-of-week, 1, 'Date.day-of-week (1701-02-28)';
is date(1700,  3,  1).day-of-week, 1, 'Date.day-of-week (1700-03-01)';
is date(1700,  2, 28).day-of-week, 7, 'Date.day-of-week (1700-02-28)';
is date(1601,  3,  1).day-of-week, 4, 'Date.day-of-week (1601-03-01)';
is dtim(1601,  3,  1).day-of-week, 4, 'DateTime.day-of-week (1601-03-01)';
is date(1601,  2, 28).day-of-week, 3, 'Date.day-of-week (1601-02-28)';
is dtim(1601,  2, 28).day-of-week, 3, 'DateTime.day-of-week (1601-02-28)';
is date(1600,  3,  1).day-of-week, 3, 'Date.day-of-week (1600-03-01)';
is date(1600,  2, 29).day-of-week, 2, 'Date.day-of-week (1600-02-29)';
is date(1600,  2, 28).day-of-week, 1, 'Date.day-of-week (1600-02-28)';

# --------------------------------------------------------------------
# L<S32::Temporal/Accessors/'The method week'>
# --------------------------------------------------------------------

is date(1977, 8, 20).week.join(' '), '1977 33', 'Date.week (1977-8-20)';
is dtim(1977, 8, 20).week.join(' '), '1977 33', 'DateTime.week (1977-8-20)';
is date(1977, 8, 20).week-year, 1977, 'Date.week (1977-8-20)';
is dtim(1977, 8, 20).week-year, 1977, 'DateTime.week (1977-8-20)';
is date(1977, 8, 20).week-number, 33, 'Date.week-number (1977-8-20)';
is dtim(1977, 8, 20).week-number, 33, 'DateTime.week-number (1977-8-20)';
is date(1987, 12, 18).week.join(' '), '1987 51', 'Date.week (1987-12-18)';
is date(2020, 5, 4).week.join(' '), '2020 19', 'Date.week (2020-5-4)';

# From http://en.wikipedia.org/w/index.php?title=ISO_week_dtim&oldid=370553706#Examples

is date(2005, 01, 01).week.join(' '), '2004 53', 'Date.week (2005-01-01)';
is date(2005, 01, 02).week.join(' '), '2004 53', 'Date.week (2005-01-02)';
is date(2005, 12, 31).week.join(' '), '2005 52', 'Date.week (2005-12-31)';
is date(2007, 01, 01).week.join(' '), '2007 1',  'Date.week (2007-01-01)';
is date(2007, 12, 30).week.join(' '), '2007 52', 'Date.week (2007-12-30)';
is dtim(2007, 12, 30).week.join(' '), '2007 52', 'DateTime.week (2007-12-30)';
is date(2007, 12, 30).week-year, 2007, 'Date.week (2007-12-30)';
is dtim(2007, 12, 30).week-year, 2007, 'DateTime.week (2007-12-30)';
is date(2007, 12, 30).week-number, 52, 'Date.week-number (2007-12-30)';
is dtim(2007, 12, 30).week-number, 52, 'DateTime.week-number (2007-12-30)';
is date(2007, 12, 31).week.join(' '), '2008 1',  'Date.week (2007-12-31)';
is date(2008, 01, 01).week.join(' '), '2008 1',  'Date.week (2008-01-01)';
is date(2008, 12, 29).week.join(' '), '2009 1',  'Date.week (2008-12-29)';
is date(2008, 12, 31).week.join(' '), '2009 1',  'Date.week (2008-12-31)';
is date(2009, 01, 01).week.join(' '), '2009 1',  'Date.week (2009-01-01)';
is date(2009, 12, 31).week.join(' '), '2009 53', 'Date.week (2009-12-31)';
is date(2010, 01, 03).week.join(' '), '2009 53', 'Date.week (2010-01-03)';
is dtim(2010, 01, 03).week.join(' '), '2009 53', 'DateTime.week (2010-01-03)';
is date(2010, 01, 03).week-year, 2009, 'Date.week-year (2010-01-03)';
is dtim(2010, 01, 03).week-year, 2009, 'DateTime.week-year (2010-01-03)';
is date(2010, 01, 03).week-number, 53, 'Date.week-number (2010-01-03)';
is dtim(2010, 01, 03).week-number, 53, 'DateTime.week-number (2010-01-03)';

# day-of-week is tested each time show-dt is called.

# --------------------------------------------------------------------
# L<S32::Temporal/Accessors/'The weekday-of-month method'>
# --------------------------------------------------------------------

is date(1982, 2,  1).weekday-of-month, 1, 'Date.weekday-of-month (1982-02-01)';
is dtim(1982, 2,  1).weekday-of-month, 1, 'DateTime.weekday-of-month (1982-02-01)';
is date(1982, 2,  7).weekday-of-month, 1, 'Date.weekday-of-month (1982-02-07)';
is date(1982, 2,  8).weekday-of-month, 2, 'Date.weekday-of-month (1982-02-08)';
is date(1982, 2, 18).weekday-of-month, 3, 'Date.weekday-of-month (1982-02-18)';
is date(1982, 2, 28).weekday-of-month, 4, 'Date.weekday-of-month (1982-02-28)';
is dtim(1982, 2, 28).weekday-of-month, 4, 'DateTime.weekday-of-month (1982-02-28)';
is date(1982, 4,  4).weekday-of-month, 1, 'Date.weekday-of-month (1982-04-04)';
is date(1982, 4,  7).weekday-of-month, 1, 'Date.weekday-of-month (1982-04-07)';
is date(1982, 4,  8).weekday-of-month, 2, 'Date.weekday-of-month (1982-04-08)';
is date(1982, 4, 13).weekday-of-month, 2, 'Date.weekday-of-month (1982-04-13)';
is date(1982, 4, 30).weekday-of-month, 5, 'Date.weekday-of-month (1982-04-30)';
is dtim(1982, 4, 30).weekday-of-month, 5, 'DateTime.weekday-of-month (1982-04-30)';

# --------------------------------------------------------------------
# L<S32::Temporal/Accessors/'The days-in-month method'>
# --------------------------------------------------------------------

is date(1999,  5,  5).days-in-month, 31, 'Date.days-in-month (May 1999)';
is date(1999,  6,  5).days-in-month, 30, 'Date.days-in-month (Jun 1999)';
is date(1999,  2,  5).days-in-month, 28, 'Date.days-in-month (Feb 1999)';
is dtim(1999,  2,  5).days-in-month, 28, 'DateTime.days-in-month (Feb 1999)';
is date(2000,  2,  5).days-in-month, 29, 'Date.days-in-month (Feb 2000)';
is dtim(2000,  2,  5).days-in-month, 29, 'DateTime.days-in-month (Feb 2000)';

# --------------------------------------------------------------------
# L<S32::Temporal/Accessors/'The day-of-year method'>
# --------------------------------------------------------------------

is date(1975,  1,  1).day-of-year,   1, 'Date.day-of-year (1975-01-01)';
is dtim(1975,  1,  1).day-of-year,   1, 'DateTime.day-of-year (1975-01-01)';
is date(1977,  5,  5).day-of-year, 125, 'Date.day-of-year (1977-05-05)';
is date(1983, 11, 27).day-of-year, 331, 'Date.day-of-year (1983-11-27)';
is date(1999,  2, 28).day-of-year,  59, 'Date.day-of-year (1999-02-28)';
is dtim(1999,  2, 28).day-of-year,  59, 'DateTime.day-of-year (1999-02-28)';
is date(1999,  3,  1).day-of-year,  60, 'Date.day-of-year (1999-03-01)';
is dtim(1999,  3,  1).day-of-year,  60, 'DateTime.day-of-year (1999-03-01)';
is date(1999, 12, 31).day-of-year, 365, 'Date.day-of-year (1999-12-31)';
is date(2000,  2, 28).day-of-year,  59, 'Date.day-of-year (2000-02-28)';
is dtim(2000,  2, 28).day-of-year,  59, 'DateTime.day-of-year (2000-02-28)';
is date(2000,  2, 29).day-of-year,  60, 'Date.day-of-year (2000-02-29)';
is dtim(2000,  2, 29).day-of-year,  60, 'DateTime.day-of-year (2000-02-29)';
is date(2000,  3,  1).day-of-year,  61, 'Date.day-of-year (2000-03-01)';
is date(2000, 12, 31).day-of-year, 366, 'Date.day-of-year (2000-12-31)';

# --------------------------------------------------------------------
# L<S32::Temporal/Accessors/'The method is-leap-year'>
# --------------------------------------------------------------------

nok date(1800,  1,  1).is-leap-year, 'Date.is-leap-year (1800)';
nok date(1801,  1,  1).is-leap-year, 'Date.is-leap-year (1801)';
ok  date(1804,  1,  1).is-leap-year, 'Date.is-leap-year (1804)';
nok date(1900,  1,  1).is-leap-year, 'Date.is-leap-year (1900)';
nok dtim(1900,  1,  1).is-leap-year, 'DateTime.is-leap-year (1900)';
ok  date(1996,  1,  1).is-leap-year, 'Date.is-leap-year (1996)';
nok date(1999,  1,  1).is-leap-year, 'Date.is-leap-year (1999)';
ok  date(2000,  1,  1).is-leap-year, 'Date.is-leap-year (2000)';
ok  dtim(2000,  1,  1).is-leap-year, 'DateTime.is-leap-year (2000)';

done;

# vim: ft=perl6
