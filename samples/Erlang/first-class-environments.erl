-module( first_class_environments ).

-export( [task/0] ).

task() ->
	Print_pid = erlang:spawn( fun() -> print_loop() end ),
	Environments = lists:seq( 1, 12 ),
	Print_pid ! "Environment:   Sequence",
	Pids = [erlang:spawn(fun() -> hailstone_in_environment(Print_pid, X) end) || X <- Environments],
	Counts = counts( Pids ),
	Print_pid ! "{Environment, Step count}",
	Print_pid ! lists:flatten( io_lib:format("~p", [Counts]) ),
	ok.



counts( Pids ) ->
	My_pid = erlang:self(),
	[X ! {count, My_pid} || X <- Pids],
	counts( Pids, [] ).
	
counts( [], Acc ) -> Acc;
counts( Pids, Acc ) ->
	receive
	{count, N, Count, Pid} -> counts( lists:delete(Pid, Pids), [{N, Count} | Acc] )
	end.

hailstone_in_environment( Print_pid, N ) ->
	erlang:put( hailstone_value, N ),
	erlang:put( count, 0 ),
	hailstone_loop( hailstone_loop_done(N), Print_pid, N, [N] ).

hailstone_loop( stop, Print_pid, N, Acc ) ->
	Environment = lists:flatten( io_lib:format("~11B:", [N]) ),
	Sequence = lists:flatten( [io_lib:format("~4B", [X]) || X <- lists:reverse(Acc)] ),
	Print_pid ! Environment ++ Sequence,
	Count= erlang:get( count ),
	receive
	{count, Pid} -> Pid ! {count, N, Count, erlang:self()}
	end;
hailstone_loop( keep_going, Print_pid, N, Acc ) ->
	Next = hailstone_next( erlang:get(hailstone_value) ),
	erlang:put( hailstone_value, Next ),
	Count = erlang:get( count ),
	erlang:put( count, Count + 1 ),
	hailstone_loop( hailstone_loop_done(Next), Print_pid, N, [Next | Acc]  ).

hailstone_loop_done( 1 ) -> stop;
hailstone_loop_done( _N ) -> keep_going.

hailstone_next( 1 ) -> 1;
hailstone_next( Even ) when (Even rem 2) =:= 0 -> Even div 2;
hailstone_next( Odd ) -> (3 * Odd) + 1.

print_loop() ->
	receive
	String -> io:fwrite("~s~n", [String] )
	end,
	print_loop().
