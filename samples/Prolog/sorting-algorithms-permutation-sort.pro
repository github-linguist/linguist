permutation_sort(L,S) :- permutation(L,S), sorted(S).

sorted([]).
sorted([_]).
sorted([X,Y|ZS]) :- X =< Y, sorted([Y|ZS]).

permutation([],[]).
permutation([X|XS],YS) :- permutation(XS,ZS), select(X,YS,ZS).
