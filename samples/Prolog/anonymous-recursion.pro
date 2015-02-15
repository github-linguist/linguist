:- use_module(lambda).

fib(N, _F) :-
	N < 0, !,
	write('fib is undefined for negative numbers.'), nl.

fib(N, F) :-
    % code of Fibonacci
    PF     = \Nb^R^Rr1^(Nb < 2 ->
			  R = Nb
                        ;
			  N1 is Nb - 1,
			  N2 is Nb - 2,
			  call(Rr1,N1,R1,Rr1),
			  call(Rr1,N2,R2,Rr1),
			  R is R1 + R2
			),

    % The Y combinator.

    Pred = PF +\Nb2^F2^call(PF,Nb2,F2,PF),

    call(Pred,N,F).
