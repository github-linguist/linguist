:- use_module(library(lambda)).


compose(F,G, FG) :-
	FG =  \X^Z^(call(G,X,Y), call(F,Y,Z)).

cube(X, Y) :-
	Y is X ** 3.

cube_root(X, Y) :-
	Y is X ** (1/3).

first_class :-
	L = [sin, cos, cube],
	IL = [asin, acos, cube_root],

	% we create the composed functions
	maplist(compose, L, IL, Lst),

	% we call the functions
	maplist(call, Lst, [0.5,0.5,0.5], R),

	% we display the results
	maplist(writeln, R).
