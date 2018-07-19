:- module sum_of_squares.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list.

main(!IO) :-
    io.write_int(sum_of_squares([3, 1, 4, 1, 5, 9]), !IO),
    io.nl(!IO).

:- func sum_of_squares(list(int)) = int.

sum_of_squares(Ns) = list.foldl((func(N, Acc) = Acc + N * N), Ns, 0).
