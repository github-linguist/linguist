:- module input_loop.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.stdin_stream(Stdin, !IO),
    io.stdout_stream(Stdout, !IO),
    read_and_print_lines(Stdin, Stdout, !IO).

:- pred read_and_print_lines(io.text_input_stream::in,
    io.text_output_stream::in, io::di, io::uo) is det.

read_and_print_lines(InFile, OutFile, !IO) :-
    io.read_line_as_string(InFile, Result, !IO),
    (
        Result = ok(Line),
        io.write_string(OutFile, Line, !IO),
        read_and_print_lines(InFile, OutFile, !IO)
    ;
        Result = eof
    ;
        Result = error(IOError),
        Msg = io.error_message(IOError),
        io.stderr_stream(Stderr, !IO),
        io.write_string(Stderr, Msg, !IO),
        io.set_exit_status(1, !IO)
    ).
