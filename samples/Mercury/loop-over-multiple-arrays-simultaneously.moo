:- module multi_array_loop.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, list, string.

main(!IO) :-
    A = ['a', 'b', 'c'],
    B = ['A', 'B', 'C'],
    C = [1, 2, 3],
    list.foldl_corresponding3(print_elems, A, B, C, !IO).

:- pred print_elems(char::in, char::in, int::in, io::di, io::uo) is det.

print_elems(A, B, C, !IO) :-
    io.format("%c%c%i\n", [c(A), c(B), i(C)], !IO).
