:- module read_entire_file.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string.

main(!IO) :-
   io.open_input("file.txt", OpenResult, !IO),
   (
      OpenResult = ok(File),
      io.read_file_as_string(File, ReadResult, !IO),
      (
           ReadResult = ok(FileContents),
           io.write_string(FileContents, !IO)
      ;
           ReadResult = error(_, IO_Error),
           io.stderr_stream(Stderr, !IO),
           io.write_string(Stderr, io.error_message(IO_Error) ++ "\n", !IO)
      )
   ;
      OpenResult = error(IO_Error),
      io.stderr_stream(Stderr, !IO),
      io.write_string(Stderr, io.error_message(IO_Error) ++ "\n", !IO)
   ).
