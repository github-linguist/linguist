:- module create_file.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module dir.

main(!IO) :-
    create_file("output.txt", !IO),
    create_file("/output.txt", !IO),
    create_dir("docs", !IO),
    create_dir("/docs", !IO).

:- pred create_file(string::in, io::di, io::uo) is det.

create_file(FileName, !IO) :-
    io.open_output(FileName, Result, !IO),
    (
        Result = ok(File),
        io.close_output(File, !IO)
    ;
        Result = error(Error),
        print_io_error(Error, !IO)
    ).

:- pred create_dir(string::in, io::di, io::uo) is det.

create_dir(DirName, !IO) :-
    dir.make_single_directory(DirName, Result, !IO),
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
