% Implemented by Arjun Sunel
-module(ternary).
-export([main/0, nott/1, andd/2,orr/2, then/2, equiv/2]).

main() ->
	{ok, [A]} = io:fread("Enter A: ","~s"),
	{ok, [B]} = io:fread("Enter B: ","~s"),
	andd(A,B).

nott(S) ->
	if
		S=="T" ->
			io : format("F\n");

	 	S=="F" ->
			io : format("T\n");

		true ->
			io: format("?\n")
	end.	
	
andd(A, B) ->
	if
		A=="T", B=="T" ->
			io : format("T\n");
		
		A=="F"; B=="F" ->
			io : format("F\n");	

		true ->
			io: format("?\n")
	end.	


orr(A, B) ->
	if
		A=="T"; B=="T" ->
			io : format("T\n");
		
		A=="?"; B=="?" ->
			io : format("?\n");	

		true ->
			io: format("F\n")
	end.
	

then(A, B) ->
	if
		B=="T" ->
			io : format("T\n");
		
		A=="?" ->
			io : format("?\n");	

		A=="F" ->
			io :format("T\n");
		B=="F" ->
			io:format("F\n");	
		true ->
			io: format("?\n")
	end.	

equiv(A, B) ->
	if
		A=="?" ->
			io : format("?\n");
		
		A=="F" ->
			io : format("~s\n", [nott(B)]);	

		true ->
			io: format("~s\n", [B])
	end.			
