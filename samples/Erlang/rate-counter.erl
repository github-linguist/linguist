-module( rate_counter ).

-export( [fun_during_seconds/2, task/0] ).

fun_during_seconds( Fun, Seconds ) ->
	My_pid = erlang:self(),
	Ref = erlang:make_ref(),
        Pid = erlang:spawn( fun() -> fun_during_seconds_loop( My_pid, Fun ) end ),
        timer:send_after( Seconds * 1000, My_pid, {stop, Ref} ),
	N = fun_during_seconds_receive_loop( Ref, Pid, 0 ),
	erlang:exit( Pid, kill ),
	N.

task() ->
    Results = [timer:tc( fun() -> io:fwrite("Hello, world!~n") end ) || _X <- lists:seq(1, 3)],
    Times = [X || {X, _Returned} <- Results],
    io:fwrite( "Times ~p, average ~p microseconds.~n", [Times, lists:sum(Times) / erlang:length(Times)]),
    N =	fun_during_seconds( fun() -> math:sqrt(123) end, 2 ),
    io:fwrite( "Square root of 123, during 2	seconds, was done ~p times.~n", [N] ).



fun_during_seconds_loop( Pid, Fun ) ->
	Fun(),
	Pid ! {one_time, erlang:self()},
	fun_during_seconds_loop( Pid, Fun ).

fun_during_seconds_receive_loop( Ref, Pid, N ) ->
	receive
	{stop, Ref} -> N;
        {one_time, Pid} -> fun_during_seconds_receive_loop( Ref, Pid, N + 1 )
	end.
