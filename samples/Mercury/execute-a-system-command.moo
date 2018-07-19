:- module execute_sys_cmd.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
   io.call_system("ls", _Result, !IO).
