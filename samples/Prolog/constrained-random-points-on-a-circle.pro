:- use_module(library(clpfd)).

circle :-
	bagof([X,Y], init(X,Y), BL),
	length(BL, N),
	length(L, 100),
	maplist(choose(BL, N), L),
	draw_circle(L).


% point selection
choose(BL, N, V) :-
	I is random(N),
	nth0(I, BL, V).

% to find all couples of numbers verifying
% 100 <= x^2 + y^2 <= 225
init(X1, Y1) :-
	X in -15..15,
	Y in -15..15,
	X*X + Y*Y #>= 100,
	X*X + Y*Y #=< 225,
	label([X,Y]),
	X1 is 10 * X + 200,
	Y1 is 10 * Y + 200.


draw_circle(L) :-
	new(D, window('Circle')),
	send(D, size,size(400,400)),
	forall(member([X,Y], L),
	       (   new(C, circle(4)),
		   send(C, fill_pattern, colour(@default, 0, 0, 0)),
		   send(C, center(point(X,Y))),
		   send(D, display, C))),
	send(D, open).
