dot_product(L1, L2, N) :-
	maplist(mult, L1, L2, P),
	sumlist(P, N).

mult(A,B,C) :-
	C is A*B.
