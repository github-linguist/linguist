:- module loops_for.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
   int.fold_up(outer_loop_body, 1, 5, !IO).

:- pred outer_loop_body(int::in, io::di, io::uo) is det.

outer_loop_body(I, !IO) :-
   int.fold_up(inner_loop_body, 1, I, !IO),
   io.nl(!IO).

:- pred inner_loop_body(int::in, io::di, io::uo) is det.

inner_loop_body(_, !IO) :-
   io.write_char('*', !IO).
