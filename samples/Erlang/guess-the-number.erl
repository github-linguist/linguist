% Implemented by Arjun Sunel
-module(guess_the_number).
-export([main/0]).

main() ->
	io:format("Guess my number between 1 and 10 until you get it right:\n"),
	N = random:uniform(10),
	guess(N).

guess(N) ->
	{ok, [K]} = io:fread("Guess number :  ","~d"),
	
	if
		K=:=N ->
			io:format("Well guessed!!\n");
		true ->
			guess(N)	
	end.	
