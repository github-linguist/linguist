%% Implemented by Arjun Sunel
-module(loop).
-export([main/0]).

main() ->
	for_loop(1).

 for_loop(N) ->
 	if N < 10 ->
		io:format("~p, ",[N] ),
		for_loop(N+1);
	true ->
		io:format("~p\n",[N])
	end.	
