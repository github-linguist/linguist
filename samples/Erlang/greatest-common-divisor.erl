% Implemented by Arjun Sunel
-module(gcd).
-export([main/0]).

main() ->gcd(-36,4).
	
gcd(A, 0) -> A;

gcd(A, B) -> gcd(B, A rem B).
