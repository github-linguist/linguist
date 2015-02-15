:- module file_io.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
   io.open_input("input.txt", InputRes, !IO),
   (
       InputRes = ok(Input),
       io.read_file_as_string(Input, ReadRes, !IO),
       (
           ReadRes = ok(Contents),
           io.close_input(Input, !IO),
           io.open_output("output.txt", OutputRes, !IO),
           (
                OutputRes = ok(Output),
                io.write_string(Output, Contents, !IO),
                io.close_output(Output, !IO)
           ;
                OutputRes = error(OutputError),
                print_io_error(OutputError, !IO)
           )
       ;
           ReadRes = error(_, ReadError),
           print_io_error(ReadError, !IO)
       )
    ;
       InputRes = error(InputError),
       print_io_error(InputError, !IO)
    ).

:- pred print_io_error(io.error::in, io::di, io::uo) is det.

print_io_error(Error, !IO) :-
   io.stderr_stream(Stderr, !IO),
   io.write_string(Stderr, io.error_message(Error), !IO),
   io.set_exit_status(1, !IO).
