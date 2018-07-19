:- module coins.
:- interface.
:- import_module int, io.
:- type coin ---> quarter; dime; nickel; penny.
:- type purse ---> purse(int, int, int, int).

:- pred sum_to(int::in, purse::out) is nondet.

:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module solutions, list, string.

:- func value(coin) = int.
value(quarter) = 25.
value(dime) = 10.
value(nickel) = 5.
value(penny) = 1.

:- pred supply(coin::in, int::in, int::out) is multi.
supply(C, Target, N) :- upto(Target div value(C), N).

:- pred upto(int::in, int::out) is multi.
upto(N, R) :- ( nondet_int_in_range(0, N, R0) -> R = R0 ; R = 0 ).

sum_to(To, Purse) :-
	Purse = purse(Q, D, N, P),
	sum(Purse) = To,
	supply(quarter, To, Q),
	supply(dime, To, D),
	supply(nickel, To, N),
	supply(penny, To, P).

:- func sum(purse) = int.
sum(purse(Q, D, N, P)) =
	value(quarter) * Q + value(dime) * D +
	value(nickel) * N + value(penny) * P.

main(!IO) :-
	solutions(sum_to(100), L),
	show(L, !IO),
	io.format("There are %d ways to make change for a dollar.\n",
                  [i(length(L))], !IO).

:- pred show(list(purse)::in, io::di, io::uo) is det.
show([], !IO).
show([P|T], !IO) :-
	io.write(P, !IO), io.nl(!IO),
	show(T, !IO).
