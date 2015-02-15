% Implemented by Arjun Sunel
-module(return_multi).
-export([main/0]).

main() ->
	K=multiply(3,4),
	C =lists:nth(1,K),
	D = lists:nth(2,K),
	E = lists:nth(3,K),
	io:format("~p~n",[C]),
	io:format("~p~n",[D]),
	io:format("~p~n",[E]).
	
multiply(A,B) ->
	case {A,B} of
		{A, B} ->[A*B, A+B, A-B]
	end.		
