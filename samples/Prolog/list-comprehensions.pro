% We need operators
:- op(700, xfx, <-).
:- op(450, xfx, ..).
:- op(1100, yfx, &).

% we need to define the intervals of numbers
Vs <- M..N :-
        integer(M),
	integer(N),
	M =< N,
	between(M, N, Vs).

% finally we define list comprehension
% prototype is Vs <- {Var, Dec, Pred} where
% Var is the list of variables to output
% Dec is the list of intervals of the variables
% Pred is the list of predicates
Vs <- {Var & Dec & Pred} :-
	findall(Var,  maplist(call, [Dec, Pred]), Vs).
