-import(lists).
-export([pascal/1]).

pascal(1)-> [[1]];
pascal(N) ->
    L = pascal(N-1),
    [H|_] = L,
    [lists:zipwith(fun(X,Y)->X+Y end,[0]++H,H++[0])|L].
