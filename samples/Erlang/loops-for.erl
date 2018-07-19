%% Implemented by Arjun Sunel
-module(nested_loops).
-export([main/0, inner_loop/0]).

main() ->
	outer_loop(1).
	
inner_loop()->
	inner_loop(1).

inner_loop(N) when N rem 5 =:= 0 ->
	io:format("* ");

inner_loop(N) ->
	io:fwrite("* "),
	inner_loop(N+1).

outer_loop(N) when N rem 5 =:= 0 ->
	io:format("*");

outer_loop(N) ->
	outer_loop(N+1),
	io:format("~n"),
	inner_loop(N).
