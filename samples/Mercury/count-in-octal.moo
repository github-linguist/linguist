:- module count_in_octal.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string.

main(!IO) :-
    count_in_octal(0, !IO).

:- pred count_in_octal(int::in, io::di, io::uo) is det.

count_in_octal(N, !IO) :-
    io.format("%o\n", [i(N)], !IO),
    count_in_octal(N + 1, !IO).
