:- module binary_digits.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string.

main(!IO) :-
    list.foldl(print_binary_digits, [5, 50, 9000], !IO).

:- pred print_binary_digits(int::in, io::di, io::uo) is det.

print_binary_digits(N, !IO) :-
    io.write_string(int_to_base_string(N, 2), !IO),
    io.nl(!IO).
