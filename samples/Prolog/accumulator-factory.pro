:- use_module(library(lambda)).

define_g(N, G) :-
	put_attr(V, user, N),
	G = V +\X^Y^(get_attr(V, user, N1),
		  Y is X + N1,
		  put_attr(V, user, Y)).

accumulator :-
	define_g(1, G),
	format('Code of g : ~w~n', [G]),
	call(G, 5, S),
	writeln(S),
	call(G, 2.3, R1),
	writeln(R1).
