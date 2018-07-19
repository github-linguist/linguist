:- module doors.
:- interface.
:- import_module array, io, int.

:- type door ---> open ; closed.
:- type doors == array(door).

:- func toggle(door) = door.
:- pred walk(int::in, doors::in, doors::out) is semidet.
:- pred walks(int::in, int::in, doors::in, doors::out) is det.

:- pred main(io::di, io::uo) is det.

:- implementation.

toggle(open) = closed.
toggle(closed) = open.

walk(N, !D) :- walk(N, N, !D).

:- pred walk(int::in, int::in, doors::in, doors::out) is semidet.
walk(At, By, !D) :-
        semidet_lookup(!.D, At - 1, Door),
        slow_set(!.D, At - 1, toggle(Door), !:D),
        ( walk(At + By, By, !D) -> true ; true ).

walks(N, End, !D) :-
        ( N =< End, walk(N, !D) -> walks(N + 1, End, !D) ; true ).

main(!IO) :-
        io.write(Doors1, !IO), io.nl(!IO),
        array.init(100, closed, Doors0),
        walks(1, 100, Doors0, Doors1).
