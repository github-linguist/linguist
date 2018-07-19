-module( generator ).

-export( [filter/2, next/1, power/1, task/0] ).

filter(	Source_pid, Remove_pid ) ->
	First_remove = next( Remove_pid	),
        erlang:spawn( fun() -> filter_loop(Source_pid, Remove_pid, First_remove) end ).

next( Pid ) ->
    Pid ! {next, erlang:self()},
    receive X -> X end.

power( M ) -> erlang:spawn( fun() -> power_loop(M, 0) end ).

task() ->
    Squares_pid = power( 2 ),
    Cubes_pid = power( 3 ),
    Filter_pid = filter( Squares_pid, Cubes_pid ),
    [next(Filter_pid) || _X <- lists:seq(1, 20)],
    [next(Filter_pid) || _X <- lists:seq(1, 10)].


filter_loop( Pid1, Pid2, N2 ) ->
        receive
        {next, Pid} ->
	       {N, New_N2} = filter_loop_next( next(Pid1), N2, Pid1, Pid2 ),
               Pid ! N
        end,
        filter_loop( Pid1, Pid2, New_N2 ).

filter_loop_next( N1, N2, Pid1, Pid2 ) when N1 > N2 -> filter_loop_next( N1, next(Pid2), Pid1, Pid2 );
filter_loop_next( N, N, Pid1, Pid2 ) -> filter_loop_next( next(Pid1), next(Pid2), Pid1, Pid2 );
filter_loop_next( N1, N2, _Pid1, _Pid2 ) -> {N1, N2}.

power_loop( M, N ) ->
        receive	{next, Pid} -> Pid ! erlang:round(math:pow(N, M) ) end,
	power_loop( M, N + 1 ).
