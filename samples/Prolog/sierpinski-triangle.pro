sierpinski_triangle(N) :-
	Len is 2 ** (N+1) - 1,
	length(L, Len),
	numlist(1, Len, LN),
	maplist(init(N), L, LN),
	atomic_list_concat(L, Line),
	writeln(Line),
	NbTours is 2**N - 1,
	loop(NbTours, LN, Len, L).

init(N, Cell, Num) :-
	(   Num is 2 ** N + 1  -> Cell = *; Cell = ' ').

loop(0, _, _, _) :- !.

loop(N, LN, Len, L) :-
	maplist(compute_next_line(Len, L), LN, L1),
	atomic_list_concat(L1, Line),
	writeln(Line),
	N1 is N - 1,
	loop(N1, LN, Len, L1).



compute_next_line(Len, L, I, V) :-
	I1 is I - 1,
	I2 is I+1,
	(   I = 1 ->  V0 = ' '; nth1(I1, L, V0)),
	nth1(I, L, V1),
	(   I = Len -> V2 = ' '; nth1(I2, L, V2)),
	rule_90(V0, V1, V2, V).

rule_90('*','*','*', ' ').
rule_90('*','*',' ', '*').
rule_90('*',' ','*', ' ').
rule_90('*',' ',' ', '*').
rule_90(' ','*','*', '*').
rule_90(' ','*',' ', ' ').
rule_90(' ',' ','*', '*').
rule_90(' ',' ',' ', ' ').
