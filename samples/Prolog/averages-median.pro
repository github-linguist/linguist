median(L, Z) :-
    length(L, Length),
    I is Length div 2,
    Rem is Length rem 2,
    msort(L, S),
    maplist(sumlist, [[I, Rem], [I, 1]], Mid),
    maplist(nth1, Mid, [S, S], X),
    sumlist(X, Y),
    Z is Y/2.
