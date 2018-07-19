:- module fizzbuzz.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module int, string, bool.

:- func fizz(int) = bool.
:- func buzz(int) = bool.
fizz(N) = ( if N mod 3 = 0 then yes else no ).
buzz(N) = ( if N mod 5 = 0 then yes else no ).

:- pred fizzbuzz(int::in, bool::in, bool::in, string::out) is det.
%           3?   5?
fizzbuzz(_, yes, yes, "FizzBuzz").
fizzbuzz(_, yes, no,  "Fizz").
fizzbuzz(_, no,  yes, "Buzz").
fizzbuzz(N, no,  no,  S) :- S = from_int(N).

main(!IO) :- main(1, 100, !IO).

:- pred main(int::in, int::in, io::di, io::uo) is det.
main(N, To, !IO) :-
	io.write_string(S, !IO), io.nl(!IO),
	fizzbuzz(N, fizz(N), buzz(N), S),
	( if N < To then main(N + 1, To, !IO) else !:IO = !.IO ).
