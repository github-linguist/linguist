floyd(N) :-
	forall(between(1, N, I),
	       (   forall(between(1,I, J),
			  (   Last is N * (N-1)/2+J,
			      V is I * (I-1) /2 + J,
			      get_column(Last, C),
			      sformat(AR, '~~t~~w~~~w| ', [C]),
			      sformat(AF, AR, [V]),
			      writef(AF))),
	       nl)).

get_column(Last, C) :-
	name(Last, N1), length(N1,C).
