kaprekar_(Z, X) :-
	split_number(Z, 10, X).


split_number(Z, N, X) :-
	N < Z,
	A is Z // N,
	B is Z mod N,
	(   (X is A+B,  B\= 0)-> true; N1 is N*10, split_number(Z, N1, X)).

kaprekar(N, V) :-
	V <- {X & X <- 1 .. N & ((Z is X * X, kaprekar_(Z, X)); X = 1) }.
