:- module string_concat.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string.

main(!IO) :-
    S = "hello",
    S1 = S ++ " world",
    io.write_string(S, !IO), io.nl(!IO),
    io.write_string(S1, !IO), io.nl(!IO).
