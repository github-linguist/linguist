catalan(N) :-
	length(L1, N),
	L = [1 | L1],
	init(1,1,L1),
	numlist(0, N, NL),
	maplist(my_write, NL, L).


init(_, _, []).

init(V, N, [H | T]) :-
	N1 is N+1,
	H is 2 * (2 * N - 1) * V / N1,
	init(H, N1, T).

my_write(N, V) :-
	format('~w : ~w~n', [N, V]).
