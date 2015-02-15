% Implemented by Arjun Sunel
-module(non_squares).
-export([main/0]).

main() ->
		lists:foreach(fun(X) -> io:format("~p~n",[non_square(X)] ) end, lists:seq(1,22)),  % First 22 non-squares.
		lists:foreach(fun(X) -> io:format("~p~n",[non_square(X)] ) end, lists:seq(1,1000000)). % First 1 million non-squares.
non_square(N) ->
	N+trunc(1/2+ math:sqrt(N)).
