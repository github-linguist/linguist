-module( fibonacci_nstep ).

-export( [nacci/2, task/0] ).

nacci( N, Ns ) when N =< erlang:length(Ns) ->
	{Sequence, _Not_sequence} = lists:split( N, Ns ),
	Sequence;
nacci( N, Ns ) ->
	Nth = erlang:length( Ns ),
	{_Nth, Sequence_reversed} = lists:foldl( fun nacci_foldl/2, {Nth, lists:reverse(Ns)}, lists:seq(Nth+1, N) ),
	lists:reverse( Sequence_reversed ).

task() ->
	Names_and_funs = [{X, fun (N) -> nacci( N, Y ) end} || {X, Y} <- [{fibonacci, [1, 1]}, {tribonacci, [1, 1, 2]}, {tetranacci, [1, 1, 2, 4]}, {lukas, [2, 1]}]],
	[io:fwrite( "~p: ~p~n", [X, Y(10)] ) || {X, Y} <- Names_and_funs].



nacci_foldl( _N, {Nth, Ns} ) ->
	{Sum_ns, _Not_sum_ns} = lists:split( Nth, Ns ),
	{Nth, [lists:sum(Sum_ns) | Ns]}.
