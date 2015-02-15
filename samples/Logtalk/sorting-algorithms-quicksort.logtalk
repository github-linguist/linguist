quicksort(List, Sorted) :-
    quicksort(List, [], Sorted).

quicksort([], Sorted, Sorted).
quicksort([Pivot| Rest], Acc, Sorted) :-
    partition(Rest, Pivot, Smaller0, Bigger0),
    quicksort(Smaller0, [Pivot| Bigger], Sorted),
    quicksort(Bigger0, Acc, Bigger).

partition([], _, [], []).
partition([X| Xs], Pivot, Smalls, Bigs) :-
    (   X @< Pivot ->
        Smalls = [X| Rest],
        partition(Xs, Pivot, Rest, Bigs)
    ;   Bigs = [X| Rest],
        partition(Xs, Pivot, Smalls, Rest)
    ).
