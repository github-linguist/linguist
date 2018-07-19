:- module rename_file.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module dir.

main(!IO) :-
    rename_file("input.txt", "output.txt", !IO),
    rename_file("docs", "mydocs", !IO),
    rename_file("/input.txt", "/output.txt", !IO),
    rename_file("/docs", "/mydocs", !IO).

:- pred rename_file(string::in, string::in, io::di, io::uo) is det.

rename_file(OldName, NewName, !IO) :-
    io.rename_file(OldName, NewName, Result, !IO),
    (
        Result = ok
    ;
        Result = error(Error),
        print_io_error(Error, !IO)
    ).

:- pred print_io_error(io.error::in, io::di, io::uo) is det.

print_io_error(Error, !IO) :-
   io.stderr_stream(Stderr, !IO),
   io.write_string(Stderr, io.error_message(Error), !IO),
   io.nl(Stderr, !IO),
   io.set_exit_status(1, !IO).
