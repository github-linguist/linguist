%% Implemented by Arjun Sunel
-module(continue).
-export([main/0, for_loop/1]).

main() ->
	for_loop(1).

 for_loop(N)  when N /= 5 , N <10 ->
	io:format("~p, ",[N] ),
	for_loop(N+1);	
	
for_loop(N) when N >=10->
 	if N=:=10 ->
		io:format("~p\n",[N] )
	end;
	
 for_loop(N) ->
 	if N=:=5 ->
		io:format("~p\n",[N] ),
		for_loop(N+1)
	end.
