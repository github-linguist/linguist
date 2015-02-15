multiple_arrays(L1, L2, L3) :-
	maplist(display, L1, L2, L3).

display(A,B,C) :-
	writef('%s%s%s\n', [[A],[B],[C]]).
