:- use_module(lambda).

compose(F,G, FG) :-
	FG =  \X^Z^(call(G,X,Y), call(F,Y,Z)).
