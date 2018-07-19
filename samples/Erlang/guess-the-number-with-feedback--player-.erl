% Implemented by Arjun Sunel
-module(guess_game).
-export([main/0]).

main() ->
	L = 1 ,	 	% Lower Limit
	U = 100, 	  % Upper Limit
	
	io:fwrite("Player 1 : Guess my number between ~p and ", [L]),
	io:fwrite("and ~p until you get it right.\n", [U]),
	N = random:uniform(100),
	guess(L,U,N).

guess(L,U,N) ->
	K = (L+U) div 2,
	io:format("Player 2 : Number guessed : ~p~n",[K]),
	if
		K=:=N ->
			io:format("Well guessed!! by Player 2\n");
		true ->
			if
				K > N ->
					io:format("Player 1 : Your guess is too high!\n"),
					guess(L,K,N);
				true ->
					io:format("Player 1 : Your guess is too low!\n"),
					guess(K,U,N)
			end			
	end.
