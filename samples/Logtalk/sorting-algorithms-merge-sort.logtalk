msort([], []) :- !.
msort([X], [X]) :- !.
msort([X, Y| Xs], Ys) :-
    split([X, Y| Xs], X1s, X2s),
    msort(X1s, Y1s),
    msort(X2s, Y2s),
    merge(Y1s, Y2s, Ys).

split([], [], []).
split([X| Xs], [X| Ys], Zs) :-
    split(Xs, Zs, Ys).

merge([X| Xs], [Y| Ys], [X| Zs]) :-
    X @=< Y, !,
    merge(Xs, [Y| Ys], Zs).
merge([X| Xs], [Y| Ys], [Y| Zs]) :-
    X @> Y, !,
    merge([X | Xs], Ys, Zs).
merge([], Xs, Xs) :- !.
merge(Xs, [], Xs).
