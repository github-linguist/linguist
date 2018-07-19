%% @author Jan Willem Luiten <jwl@secondmove.com>
%% Hofstadter Q Sequence for Rosetta Code

-module(hofstadter).
-export([main/0]).
-define(MAX, 100000).

flip(V2, V1) when V1 > V2 -> 1;
flip(_V2, _V1) -> 0.
	
list_terms(N, N, Acc) ->
	io:format("~w~n", [array:get(N, Acc)]);
list_terms(Max, N, Acc) ->
	io:format("~w, ", [array:get(N, Acc)]),
	list_terms(Max, N+1, Acc).

hofstadter(N, N, Acc, Flips) ->
	io:format("The first ten terms are: "),
	list_terms(9, 0, Acc),
	io:format("The 1000'th term is ~w~n", [array:get(999, Acc)]),
	io:format("Number of flips: ~w~n", [Flips]);
hofstadter(Max, N, Acc, Flips) ->
	Qn1 = array:get(N-1, Acc),
	Qn = array:get(N - Qn1, Acc) + array:get(N - array:get(N-2, Acc), Acc),
	hofstadter(Max, N+1, array:set(N, Qn, Acc), Flips + flip(Qn, Qn1)).

main() ->
	Tmp = array:set(0, 1, array:new(?MAX)),
	Acc = array:set(1, 1, Tmp),
	hofstadter(?MAX, 2, Acc, 0).
