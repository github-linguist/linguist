lcm(X, Y, Z) :-
	Z is abs(X * Y) / gcd(X,Y).
