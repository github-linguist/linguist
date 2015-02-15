:- module sort_int_list.
:- interface.
:- import_module io.

:- pred main(io::di, uo::uo) is det.

:- implementation.
:- import_module list.

main(!IO) :-
  Nums = [2, 4, 0, 3, 1, 2],
  list.sort(Nums, Sorted),
  io.write(Sorted, !IO),
  io.nl(!IO).
