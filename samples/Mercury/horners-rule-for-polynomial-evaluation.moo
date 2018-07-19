:- module horner.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module int, list, string.

main(!IO) :-
    io.format("%i\n", [i(horner(3, [-19, 7, -4, 6]))], !IO).

:- func horner(int, list(int)) = int.

horner(X, Cs) = list.foldr((func(C, Acc) = Acc * X + C), Cs, 0).
