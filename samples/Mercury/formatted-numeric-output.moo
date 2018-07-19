:- module formatted_numeric_output.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, string.

main(!IO) :-
    io.format("%09.3f\n", [f(7.125)], !IO).
