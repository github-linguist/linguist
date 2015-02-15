flatten(List, Flatted) :-
    flatten(List, [], Flatted).

flatten(Var, Tail, [Var| Tail]) :-
    var(Var),
    !.
flatten([], Flatted, Flatted) :-
    !.
flatten([Head| Tail], List, Flatted) :-
    !,
    flatten(Tail, List, Aux),
    flatten(Head, Aux, Flatted).
flatten(Head, Tail, [Head| Tail]).
