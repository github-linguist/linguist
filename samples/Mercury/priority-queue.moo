:- module test_pqueue.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module pqueue.
:- import_module string.

:- pred build_pqueue(pqueue(int,string)::in, pqueue(int,string)::out) is det.
build_pqueue(!PQ) :-
  pqueue.insert(3, "Clear drains",   !PQ),
  pqueue.insert(4, "Feed cat",       !PQ),
  pqueue.insert(5, "Make tea",       !PQ),
  pqueue.insert(1, "Solve RC tasks", !PQ),
  pqueue.insert(2, "Tax return",     !PQ).

:- pred display_pqueue(pqueue(int, string)::in, io::di, io::uo) is det.
display_pqueue(PQ, !IO) :-
  ( pqueue.remove(K, V, PQ, PQO) ->
      io.format("Key = %d, Value = %s\n", [i(K), s(V)], !IO),
      display_pqueue(PQO, !IO)
  ;
      true
  ).

main(!IO) :-
  build_pqueue(pqueue.init, PQO),
  display_pqueue(PQO, !IO).
