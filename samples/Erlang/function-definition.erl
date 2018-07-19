% Implemented by Arjun Sunel
-module(func_definition).
-export([main/0]).

main() ->
	K=multiply(3,4),
	io :format("~p~n",[K]).
	
multiply(A,B) ->
	case {A,B} of
		{A, B} -> A * B
	end.
