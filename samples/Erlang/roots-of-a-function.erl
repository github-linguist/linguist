% Implemented by Arjun Sunel
-module(roots).
-export([main/0]).
main() ->
	F = fun(X)->X*X*X - 3*X*X + 2*X end,
	Step = 0.001,	 % Using smaller steps will provide more accurate results
	Start = -1,
	Stop = 3,
	Sign = F(Start) > 0,
	X = Start,
	while(X, Step, Start, Stop, Sign,F).

while(X, Step, Start, Stop, Sign,F) ->	
	Value = F(X),
	if
		Value == 0  ->		% We hit a root
        	io:format("Root found at ~p~n",[X]),
        	while(X+Step, Step, Start, Stop,  Value > 0,F);

		(Value < 0) == Sign ->	% We passed a root
		io:format("Root found near ~p~n",[X]),
		while(X+Step , Step, Start, Stop,  Value > 0,F);
		
		 X > Stop ->
		 	io:format("") ;
		true ->
        	while(X+Step, Step, Start, Stop,  Value > 0,F)
	end.	
