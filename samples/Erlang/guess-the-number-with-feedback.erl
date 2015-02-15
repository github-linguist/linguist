% Implemented by Arjun Sunel
-module(guess_number).
-export([main/0]).

main() ->
	L = 1 ,	 	% Lower Limit
	U = 100, 	  % Upper Limit
	
	io:fwrite("Guess my number between ~p and ", [L]),
	io:fwrite("and ~p until you get it right.\n", [U]),
	N = random:uniform(100),
	guess(N).

guess(N) ->
	{ok, [K]} = io:fread("Guess the number :  ","~d"),
	if
		K=:=N ->
			io:format("Well guessed!!\n");
		true ->
			if
				K > N ->
					io:format("Your guess is too high.\n");
				true ->
					io:format("Your guess is too low.\n")
			end,			
			guess(N)	
	end.	
