:- module partial_function_application.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list.

main(!IO) :-
    io.write((fsf1)([0, 1, 2, 3]), !IO), io.nl(!IO),
    io.write((fsf2)([0, 1, 2, 3]), !IO), io.nl(!IO),
    io.write((fsf1)([2, 4, 6, 8]), !IO), io.nl(!IO),
    io.write((fsf2)([2, 4, 6, 8]), !IO), io.nl(!IO).

:- func fs(func(V) = V, list(V)) = list(V).

fs(_, []) = [].
fs(F, [V | Vs]) = [F(V) | fs(F, Vs)].

:- func f1(int) = int.

f1(V) = V * 2.

:- func f2(int) = int.

f2(V) = V * V.

:- func fsf1 = (func(list(int)) = list(int)).

fsf1 = fs(f1).

:- func fsf2 = (func(list(int)) = list(int)).

fsf2 = fs(f2).
