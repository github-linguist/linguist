-module(hailstone).
-import(io).
-export([main/0]).

hailstone(1) -> [1];
hailstone(N) when N band 1 == 1 -> [N|hailstone(N * 3 + 1)];
hailstone(N) when N band 1 == 0 -> [N|hailstone(N div 2)].

max_length(Start, Stop) ->
    F = fun (N) -> {length(hailstone(N)), N} end,
    Lengths = lists:map(F, lists:seq(Start, Stop)),
    lists:max(Lengths).

main() ->
    io:format("hailstone(4): ~w~n", [hailstone(4)]),
    Seq27 = hailstone(27),
    io:format("hailstone(27) length: ~B~n", [length(Seq27)]),
    io:format("hailstone(27) first 4: ~w~n",
              [lists:sublist(Seq27, 4)]),
    io:format("hailstone(27) last 4: ~w~n",
              [lists:nthtail(length(Seq27) - 4, Seq27)]),
    io:format("finding maximum hailstone(N) length for 1 <= N <= 100000..."),
    {Length, N} = max_length(1, 100000),
    io:format(" done.~nhailstone(~B) length: ~B~n", [N, Length]).
