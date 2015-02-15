:- module pythtrip.
:- interface.
:- import_module io.
:- import_module int.

:- type triple ---> triple(int, int, int).

:- pred pythTrip(int::in,triple::out) is nondet.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.
:- import_module solutions.
:- import_module math.

pythTrip(Limit,triple(X,Y,Z)) :-
    nondet_int_in_range(1,Limit,X),
    nondet_int_in_range(X,Limit,Y),
    nondet_int_in_range(Y,Limit,Z),
    pow(Z,2) = pow(X,2) + pow(Y,2).

main(!IO) :-
     solutions((pred(Triple::out) is nondet :- pythTrip(20,Triple)),Result),
     write(Result,!IO).
