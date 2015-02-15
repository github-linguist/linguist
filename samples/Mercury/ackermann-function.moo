:- func ack(integer, integer) = integer.
ack(M, N) = R :- ack(M, N, R).

:- pred ack(integer::in, integer::in, integer::out) is det.
ack(M, N, R) :-
	( ( M < integer(0)
	  ; N < integer(0) ) -> throw(bounds_error)
	; M = integer(0)     -> R = N + integer(1)
	; N = integer(0)     -> ack(M - integer(1), integer(1), R)
	;                       ack(M - integer(1), ack(M, N - integer(1)), R) ).
