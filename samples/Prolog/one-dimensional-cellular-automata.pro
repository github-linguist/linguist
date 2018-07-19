one_dimensional_cellular_automata(L) :-
	maplist(my_write, L), nl,
	length(L, N),
	length(LN, N),
	% there is a 0 before the beginning
	compute_next([0 |L], LN),
	(   L \= LN -> one_dimensional_cellular_automata(LN); true).

% All the possibilites
compute_next([0, 0, 0 | R], [0 | R1]) :-
	compute_next([0, 0 | R], R1).

compute_next([0, 0, 1 | R], [0 | R1]) :-
	compute_next([0, 1 | R], R1).

compute_next([0, 1, 0 | R], [0 | R1]) :-
	compute_next([1, 0 | R], R1).

compute_next([0, 1, 1 | R], [1 | R1]) :-
	compute_next([1, 1 | R], R1).

compute_next([1, 0, 0 | R], [0 | R1]) :-
	compute_next([0, 0 | R], R1).

compute_next([1, 0, 1 | R], [1 | R1]) :-
	compute_next([0, 1 | R], R1).

compute_next([1, 1, 0 | R], [1 | R1]) :-
	compute_next([1, 0 | R], R1).

compute_next([1, 1, 1 | R], [0 | R1]) :-
	compute_next([1, 1 | R], R1).

% the last four possibilies =>
% we consider that there is à 0  after the end
compute_next([0, 0], [0]).

compute_next([1, 0], [0]).

compute_next([0, 1], [0]).

compute_next([1, 1], [1]).

my_write(0) :-
	write(.).

my_write(1) :-
	write(#).

one_dimensional_cellular_automata :-
	L = [0,1,1,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,0],
	one_dimensional_cellular_automata(L).
