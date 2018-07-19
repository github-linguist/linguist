% transposition of a rectangular matrix
% e.g.   [[1,2,3,4], [5,6,7,8]]
% give [[1,5],[2,6],[3,7],[4,8]]

transpose(In, Out) :-
    In = [H | T],
    maplist(initdl, H, L),
    work(T, In, Out).

% we use the difference list to make "quick" appends (one inference)
initdl(V, [V | X] - X).

work(Lst, [H], Out) :-
	maplist(my_append_last, Lst, H, Out).

work(Lst, [H | T], Out) :-
    maplist(my_append, Lst, H, Lst1),
    work(Lst1, T, Out).

my_append(X-Y, C, X1-Y1) :-
    append_dl(X-Y, [C | U]- U, X1-Y1).

my_append_last(X-Y, C, X1) :-
	append_dl(X-Y, [C | U]- U, X1-[]).

% "quick" append
append_dl(X-Y, Y-Z, X-Z).
