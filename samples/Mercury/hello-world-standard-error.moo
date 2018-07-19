:- module hello_error.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.stderr_stream(Stderr, !IO),
    io.write_string(Stderr, "Goodbye, World!\n", !IO).
