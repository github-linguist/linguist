 ?- assert((fun(X, Y) :- Y is 2 * X)).
true.

?- maplist(fun, [1,2,3,4,5], L).
L = [2,4,6,8,10].
