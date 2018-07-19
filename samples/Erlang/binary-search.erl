%% Task: Binary Search algorithm
%% Author: Abhay Jain

-module(searching_algorithm).
-export([start/0]).

start() ->
    List = [1,2,3],
    binary_search(List, 5, 1, length(List)).


binary_search(List, Value, Low, High) ->
    if Low > High ->
        io:format("Number ~p not found~n", [Value]),
        not_found;
       true ->
        Mid = (Low + High) div 2,
        MidNum = lists:nth(Mid, List),
        if MidNum > Value ->
            binary_search(List, Value, Low, Mid-1);
           MidNum < Value ->
            binary_search(List, Value, Mid+1, High);
           true ->
            io:format("Number ~p found at index ~p", [Value, Mid]),
            Mid
        end
    end.
