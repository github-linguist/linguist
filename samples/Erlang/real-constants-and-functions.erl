% Implemented by Arjun Sunel
-module(math_constants).
-export([main/0]).
main() ->
	io:format("~p~n",	[math:exp(1)]		),		% e
	io:format("~p~n",	[math:pi()]		),		% pi
	io:format("~p~n",	[math:sqrt(16)]		),		% square root
	io:format("~p~n",	[math:log(10)]		),		% natural logarithm			
	io:format("~p~n",	[math:log10(10)]	),		% base 10 logarithm
	io:format("~p~n",	[math:exp(2)]		),		% e raised to the power of x
	io:format("~p~n",	[abs(-2.24)]		),		% absolute value
	io:format("~p~n",	[floor(3.1423)]		),		% floor
	io:format("~p~n",	[ceil(20.125)]		),		% ceiling
	io:format("~p~n",	[math:pow(3,2)]	        ).		% exponentiation

floor(X) when X < 0 ->
	T = trunc(X),
	case X - T == 0 of
		true -> T;
        	false -> T - 1
	end;

floor(X) ->
	trunc(X).


ceil(X) when X < 0 ->
	trunc(X);

ceil(X) ->
	T = trunc(X),
	case X - T == 0 of
		true -> T;
		false -> T + 1
	end.	
