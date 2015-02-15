% Implemented by Arjun Sunel
-module(lcm).
-export([main/0]).

main() ->
	lcm(-3,4).
	
gcd(A, 0) ->
	A;

gcd(A, B) ->
	gcd(B, A rem B).

lcm(A,B) ->
	abs(A*B div gcd(A,B)).
