max([X| Xs], Max) :-
    max(Xs, X, Max).

max([], Max, Max).
max([X| Xs], Aux, Max) :-
    (   X @> Aux ->
        max(Xs, X, Max)
    ;   max(Xs, Aux, Max)
    ).
