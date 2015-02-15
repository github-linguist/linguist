-module( mean_angle ).
-export( [from_degrees/1, task/0] ).

from_degrees( Angles ) ->
	Radians = [radians(X) || X <- Angles],
	Sines = [math:sin(X) || X <- Radians],
	Coses = [math:cos(X) || X <- Radians],
	degrees( math:atan2( average(Sines), average(Coses) ) ).

task() ->
	Angles = [[350, 10], [90, 180, 270, 360], [10, 20, 30]],
	[io:fwrite( "Mean angle of ~p is: ~p~n", [X, erlang:round(from_degrees(X))] ) || X <- Angles].


average( List ) -> lists:sum( List ) / erlang:length( List ).

degrees( Radians ) -> Radians * 180 / math:pi().

radians( Degrees ) -> Degrees * math:pi() / 180.
