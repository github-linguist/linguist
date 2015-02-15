-module( mean_time_of_day ).
-export( [from_times/1, task/0] ).

from_times( Times ) ->
	Seconds = [seconds_from_time(X) || X <- Times],
	Degrees = [degrees_from_seconds(X) || X <- Seconds],
	Average = mean_angle:from_degrees( Degrees ),
	time_from_seconds( seconds_from_degrees(Average) ).

task() ->
	Times = ["23:00:17", "23:40:20", "00:12:45", "00:17:19"],
	io:fwrite( "The mean time of  ~p is: ~p~n", [Times, from_times(Times)] ).



degrees_from_seconds( Seconds ) when Seconds < (24 * 3600) -> (Seconds * 360) / (24 * 3600).

seconds_from_degrees( Degrees ) when Degrees < 0 -> seconds_from_degrees( Degrees + 360 );
seconds_from_degrees( Degrees ) when Degrees < 360 -> (Degrees * 24 * 3600) / 360.

seconds_from_time( Time ) ->
	{ok, [Hours, Minutes, Seconds], _Rest} = io_lib:fread( "~d:~d:~d", Time ),
	Hours * 3600 + Minutes * 60 + Seconds.

time_from_seconds( Seconds_float ) ->
	Seconds = erlang:round( Seconds_float ),
	Hours = Seconds div 3600,
	Minutes = (Seconds - (Hours * 3600)) div 60,
	Secs = Seconds - (Hours * 3600) - (Minutes * 60),
	lists:flatten( io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B", [Hours, Minutes, Secs]) ).
