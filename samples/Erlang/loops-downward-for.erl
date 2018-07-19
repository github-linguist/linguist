%% Implemented by Arjun Sunel
-module(downward_loop).
-export([main/0]).

main() ->
	for_loop(10).

 for_loop(N) ->
 	if N > 0 ->
		io:format("~p~n",[N] ),
		for_loop(N-1);
	true ->
		io:format("~p~n",[N])
	end.	
