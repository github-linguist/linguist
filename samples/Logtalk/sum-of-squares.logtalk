sum(List, Sum) :-
    sum(List, 0, Sum).

sum([], Sum, Sum).
sum([X| Xs], Acc, Sum) :-
    Acc2 is Acc + X,
    sum(Xs, Acc2, Sum).
