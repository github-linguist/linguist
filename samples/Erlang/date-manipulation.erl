-module( date_manipulation ).

-export( [task/0] ).

task() ->
	{Date_time, TZ} = date_time_tz_from_string( "March 7 2009 7:30pm EST" ),
	Seconds1 = calendar:datetime_to_gregorian_seconds( Date_time ),
	Seconds2 = calendar:datetime_to_gregorian_seconds( {calendar:gregorian_days_to_date(0), {12, 0, 0}} ),
	Date_time_later = calendar:gregorian_seconds_to_datetime( Seconds1 + Seconds2 ),
	{Date_time_later, TZ}.



date_time_tz_from_string( String ) ->
	[Month, Date, Year, Time, TZ] = string:tokens( String, " " ),
	[Hour, Minute] = string:tokens( Time, ":" ),
	{{date_from_strings(Year, Month, Date), time_from_strings(Hour, Minute)}, TZ}.

date_from_strings( Year, Month, Date ) ->
	{erlang:list_to_integer(Year), date_from_strings_month(Month), erlang:list_to_integer(Date)}.

date_from_strings_month( "January" ) -> 1;
date_from_strings_month( "February" ) -> 2;
date_from_strings_month( "March" ) -> 3;
date_from_strings_month( "April" ) -> 4;
date_from_strings_month( "May" ) -> 5;
date_from_strings_month( "June" ) -> 6;
date_from_strings_month( "July" ) -> 7;
date_from_strings_month( "August" ) -> 8;
date_from_strings_month( "September" ) -> 9;
date_from_strings_month( "October" ) -> 10;
date_from_strings_month( "November" ) -> 11;
date_from_strings_month( "December" ) -> 12.

time_from_strings( Hour, Minute_12hours ) ->
	{ok, [Minute], AM_PM} = io_lib:fread("~d", Minute_12hours ),
	{time_from_strings_hour( Hour, string:to_lower(AM_PM) ), Minute, 0}.

time_from_strings_hour( Hour, "am" ) -> erlang:list_to_integer( Hour );
time_from_strings_hour( Hour, "pm" ) -> erlang:list_to_integer( Hour ) + 12.
