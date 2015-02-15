:- module notes.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, time.

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if Args = [] then print_notes(!IO) else add_note(Args, !IO) ).

:- pred print_notes(io::di, io::uo) is det.

print_notes(!IO) :-
   io.open_input(notes_filename, InputRes, !IO),
   (
        InputRes = ok(Input),
        io.input_stream_foldl_io(Input, io.write_char, WriteRes, !IO),
        (
            WriteRes = ok
        ;
            WriteRes = error(WriteError),
            print_io_error(WriteError, !IO)
        ),
        io.close_input(Input, !IO)
   ;
        InputRes = error(_InputError)
   ).

:- pred add_note(list(string)::in, io::di, io::uo) is det.

add_note(Words, !IO) :-
   io.open_append(notes_filename, OutputRes, !IO),
   (
       OutputRes = ok(Output),
       time(Time, !IO),
       io.write_string(Output, ctime(Time), !IO),
       io.write_char(Output, '\t', !IO),
       io.write_list(Output, Words, " ", io.write_string, !IO),
       io.nl(Output, !IO),
       io.close_output(Output, !IO)
   ;
       OutputRes = error(OutputError),
       print_io_error(OutputError, !IO)
   ).

:- pred print_io_error(io.error::in, io::di, io::uo) is det.

print_io_error(Error, !IO) :-
   io.stderr_stream(Stderr, !IO),
   io.write_string(Stderr, io.error_message(Error), !IO),
   io.set_exit_status(1, !IO).

:- func notes_filename = string.

notes_filename = "NOTES.TXT".

:- end_module notes.
