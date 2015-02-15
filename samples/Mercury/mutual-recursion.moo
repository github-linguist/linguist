:- module mutual_recursion.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list.

main(!IO) :-
   io.write(list.map(f, 0..19), !IO), io.nl(!IO),
   io.write(list.map(m, 0..19), !IO), io.nl(!IO).

:- func f(int) = int.

f(N) = ( if N = 0 then 1 else N - m(f(N - 1)) ).

:- func m(int) = int.

m(N) = ( if N = 0 then 0 else N - f(m(N - 1)) ).
