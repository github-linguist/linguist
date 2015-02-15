-module(str).
-export([look_and_say/1, look_and_say/2]).

%% converts a single number
look_and_say([H|T]) -> lists:reverse(look_and_say(T,H,1,"")).

%% converts and accumulates as a loop
look_and_say(_, 0) -> [];
look_and_say(Start, Times) when Times > 0 ->
    [Start | look_and_say(look_and_say(Start), Times-1)].

%% does the actual conversion for a number
look_and_say([], Current, N, Acc) ->
    [Current, $0+N | Acc];
look_and_say([H|T], H, N, Acc) ->
    look_and_say(T, H, N+1, Acc);
look_and_say([H|T], Current, N, Acc) ->
    look_and_say(T, H, 1, [Current, $0+N | Acc]).
