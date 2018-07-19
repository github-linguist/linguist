-module(sierpinski).
-export([triangle/1]).

triangle(N) ->
    F = fun(X) -> io:format("~s~n",[X]) end,
    lists:foreach(F, triangle(N, ["*"], " ")).

triangle(0, Down, _) -> Down;
triangle(N, Down, Sp) ->
    NewDown = [Sp++X++Sp || X<-Down]++[X++" "++X || X <- Down],
    triangle(N-1, NewDown, Sp++Sp).
