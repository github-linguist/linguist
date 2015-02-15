-module( standard_deviation ).

-export( [add_sample/2, create/0, destroy/1, get/1, task/0] ).

-compile({no_auto_import,[get/1]}).

add_sample( Pid, N ) -> Pid ! {add, N}.

create() -> erlang:spawn_link( fun() -> loop( [] ) end ).

destroy( Pid ) -> Pid ! stop.

get( Pid ) ->
	Pid ! {get, erlang:self()},
	receive
	{get, Value, Pid} -> Value
	end.

task() ->
	Pid = create(),
	[add_print(Pid, X, add_sample(Pid, X)) || X <- [2,4,4,4,5,5,7,9]],
	destroy( Pid ).



add_print( Pid, N, _Add ) -> io:fwrite( "Standard deviation ~p when adding ~p~n", [get(Pid), N] ).

loop( Ns ) ->
	receive
	{add, N} -> loop( [N | Ns] );
	{get, Pid} ->
		Pid ! {get, loop_calculate( Ns ), erlang:self()},
		loop( Ns );
	stop -> ok
	end.

loop_calculate( Ns ) ->
	Average = loop_calculate_average( Ns ),
	math:sqrt( loop_calculate_average([math:pow(X - Average, 2) || X <- Ns]) ).

loop_calculate_average( Ns ) -> lists:sum( Ns ) / erlang:length( Ns ).
