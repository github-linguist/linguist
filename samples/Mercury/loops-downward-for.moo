:- module loops_downward_for.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
   Print = (pred(I::in, !.IO::di, !:IO::uo) is det :-
       io.write_int(I, !IO), io.nl(!IO)
   ),
   int.fold_down(Print, 1, 10, !IO).
