:- use_module(library(lambda)).

niven :-
	nb_setval(go, 1),

	L = [1 | _],
	print_niven(L, 1),
	gen_niven(1, L).


print_niven([X|T], N) :-
	when(ground(X),
	     (	 (   nb_getval(go, 1)
		 ->  (   N < 20
		     ->  writeln(X),
			 N1 is N+1,
			 print_niven(T, N1)
		     ;	 (   X > 1000
			 ->  writeln(X),
			     nb_setval(go, 0)
			 ;   N1 is N+1,
			     print_niven(T, N1)))
		 ;   true))).



gen_niven(X, [N | T]) :-
	(   nb_getval(go, 1)
	->  X1 is X+1,
	    sum_of_digit(X, S),
	    (   X mod S =:= 0
	    ->  N = X,
		gen_niven(X1, T)
	    ;	gen_niven(X1, [N | T]))
	;   true).


sum_of_digit(N, S) :-
	number_chars(N, LC),
	maplist(\X^Y^number_chars(Y, [X]), LC, LN),
	sum_list(LN, S).
