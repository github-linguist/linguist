-module(while).
-export([loop/0]).

loop() ->
	loop(1024).

loop(N) when N div 2 =:= 0 ->
	io:format("~w~n", [N]);
	
loop(N) when N >0 ->
	io:format("~w~n", [N]),
	loop(N div 2).
