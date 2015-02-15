-module(format_date).
-export([iso_date/0, iso_date/1, iso_date/3, long_date/0, long_date/1, long_date/3]).
-import(calendar,[day_of_the_week/1]).
-import(io,[format/2]).
-import(lists,[append/1]).

iso_date() -> iso_date(date()).
iso_date(Year, Month, Day) -> iso_date({Year, Month, Day}).
iso_date(Date) ->
  format("~4B-~2..0B-~2..0B~n", tuple_to_list(Date)).

long_date() -> long_date(date()).
long_date(Year, Month, Day) -> long_date({Year, Month, Day}).
long_date(Date = {Year, Month, Day}) ->
  Months =   { "January",   "February", "March",    "April",
               "May",       "June",     "July",     "August",
               "September", "October",  "November", "December" },
  Weekdays = { "Monday", "Tuesday",  "Wednesday", "Thursday",
               "Friday", "Saturday", "Sunday" },
  Weekday     = day_of_the_week(Date),
  WeekdayName = element(Weekday, Weekdays),
  MonthName   = element(Month, Months),
  append([WeekdayName, ", ", MonthName, " ", integer_to_list(Day), ", ",
          integer_to_list(Year)]).
