:- use_module(library(lists)).

set :-
	A = [2, 4, 1, 3],
	B = [5, 2, 3, 2],
	(   is_set(A) -> format('~w is a set~n', [A])
	;   format('~w is not a set~n', [A])),
	(   is_set(B) -> format('~w is a set~n', [B])
	;   format('~w is not a set~n', [B])),

	% create a set from a list

	list_to_set(B, BS),
	(   is_set(BS) -> format('~nCreate a set from a list~n~w is a set~n', [BS])
	;   format('~w is not a set~n', [BS])),

	intersection(A, BS, I),
	format('~n~w intersection ~w => ~w~n', [A, BS, I]),
	union(A, BS, U),
	format('~w union ~w => ~w~n', [A, BS, U]),
	difference(A, BS, D),
	format('~w difference ~w => ~w~n', [A, BS, D]),

	X = [1,2],
	(   subset(X, A) -> format('~n~w is a subset of ~w~n', [X, A])
	;   format('~w is not a subset of ~w~n', [X, A])),
	Y = [1,5],
	(   subset(Y, A) -> format('~w is a subset of ~w~n', [Y, A])
	;   format('~w is not a subset of ~w~n', [Y, A])),
	Z = [1, 2, 3, 4],
	(  equal(Z, A) -> format('~n~w is equal to ~w~n', [Z, A])
	;   format('~w is not equal to ~w~n', [Z, A])),
	T = [1, 2, 3],
	(  equal(T, A) -> format('~w is equal to ~w~n', [T, A])
	;   format('~w is not equal to ~w~n', [T, A])).



% compute difference of sets
difference(A, B, D) :-
	exclude(member_(B), A, D).

member_(L, X) :-
	member(X, L).

equal([], []).
equal([H1 | T1], B) :-
	select(H1, B, B1),
	equal(T1, B1).
