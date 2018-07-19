% Implemented by Arjun Sunel
-module(carpet).
-export([main/0]).

main() ->
	sierpinski_carpet(3).

sierpinski_carpet(N) ->
	lists: foreach(fun(X) -> lists: foreach(fun(Y) -> carpet(X,Y) end,lists:seq(0,trunc(math:pow(3,N))-1)), io:format("\n") end, lists:seq(0,trunc(math:pow(3,N))-1)).	

carpet(X,Y) ->
	if
		X=:=0 ; Y=:=0 ->
			io:format("*");
		(X rem 3)=:=1, (Y rem 3) =:=1 ->	
			io:format(" ");
		true ->
			carpet(X div 3, Y div 3)	
	end.
