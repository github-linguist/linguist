factor(N, L) :-
	factor(N, 1, [], L).

factor(N, X, LC, L) :-
	0 is N mod X,
	!,
	Q is N / X,
	(Q = X ->
	    sort([Q | LC], L)
	;
	    (Q > X ->
	       X1 is X+1,
	       factor(N, X1, [X, Q|LC], L)
	    ;
	      sort(LC, L)
	    )
	).

factor(N, X, LC, L) :-
	Q is N / X,
	(Q > X ->
	    X1 is X+1,
	    factor(N, X1, LC, L)
	;
	    sort(LC, L)
	).
