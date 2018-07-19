-module( short_circuit_evaluation ).

-export( [task/0] ).

task() ->
	[task_helper(X, Y) || X <- [true, false], Y <- [true, false]].



a( Boolean ) ->
	io:fwrite( " a ~p~n", [Boolean] ),
	Boolean.

b( Boolean ) ->
	io:fwrite( " b ~p~n", [Boolean] ),
	Boolean.

task_helper( Boolean1, Boolean2 ) ->
	io:fwrite( "~p andalso ~p~n", [Boolean1, Boolean2] ),
	io:fwrite( "=> ~p~n", [a(Boolean1) andalso b(Boolean2)] ),
	io:fwrite( "~p orelse ~p~n", [Boolean1, Boolean2] ),
	io:fwrite( "=> ~p~n", [a(Boolean1) orelse b(Boolean2)] ).
