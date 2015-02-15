-module(forward_difference).
-export([difference/1, difference/2]).

-export([task/0]).
-define(TEST_DATA,[90, 47, 58, 29, 22, 32, 55, 5, 55, 73]).

difference([X|Xs]) ->
    {Result,_} = lists:mapfoldl(fun (N_2,N_1) -> {N_2 - N_1, N_2} end, X, Xs),
    Result.

difference([],_) -> [];
difference(List,0) -> List;
difference(List,Order) -> difference(difference(List),Order-1).

task() ->
    io:format("Initial: ~p~n",[?TEST_DATA]),
    [io:format("~3b: ~p~n",[N,difference(?TEST_DATA,N)]) || N <- lists:seq(0,length(?TEST_DATA))],
    ok.
