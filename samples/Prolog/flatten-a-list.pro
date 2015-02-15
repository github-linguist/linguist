flatten(List, FlatList) :-
	flatten(List, [], FlatList).

flatten(Var, T, [Var|T]) :-
	var(Var), !.
flatten([], T, T) :- !.
flatten([H|T], TailList, List) :- !,
	flatten(H, FlatTail, List),
	flatten(T, TailList, FlatTail).

flatten(NonList, T, [NonList|T]).
