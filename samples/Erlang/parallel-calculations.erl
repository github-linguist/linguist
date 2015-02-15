-module( parallel_calculations ).

-export( [fun_results/2, task/0] ).

fun_results( Fun, Datas ) ->
        My_pid = erlang:self(),
	Pids = [fun_spawn( Fun, X, My_pid ) || X <- Datas],
	[fun_receive(X) || X <- Pids].

task() ->
    Numbers = [12757923, 12878611, 12757923, 15808973, 15780709, 197622519],
    Results = fun_results( fun factors/1, Numbers ),
    Min_results = [lists:min(X) || X <- Results],
    {_Max_min_factor, Number} = lists:max( lists:zip(Min_results, Numbers) ),
    {Number, Factors} = lists:keyfind( Number, 1, lists:zip(Numbers, Results) ),
    io:fwrite( "~p has largest minimal factor among its prime factors ~p~n", [Number, Factors] ).



factors(N) -> factors(N,2,[]).

factors(1,_,Acc) -> Acc;
factors(N,K,Acc) when N rem K == 0 -> factors(N div K,K, [K|Acc]);
factors(N,K,Acc) -> factors(N,K+1,Acc).

fun_receive( Pid ) ->
        receive
        {ok, Result, Pid} -> Result;
	{Type, Error, Pid} -> erlang:Type( Error )
        end.

fun_spawn( Fun, Data, My_pid ) ->
        erlang:spawn( fun() ->
                Result = try
                       {ok, Fun(Data), erlang:self()}

		       catch
	               Type:Error -> {Type, Error, erlang:self()}

		       end,
	        My_pid ! Result
        end ).
