-module(index).
-export([main/0]).

main() ->
    Haystack = ["Zig","Zag","Wally","Ronald","Bush","Krusty","Charlie","Bush","Bozo"],
    Needles = ["Washington","Bush"],
    lists:foreach(fun ?MODULE:print/1, [{N,pos(N, Haystack)} || N <- Needles]).

pos(Needle, Haystack) -> pos(Needle, Haystack, 1).
pos(_, [], _) -> undefined;
pos(Needle, [Needle|_], Pos) -> Pos;
pos(Needle, [_|Haystack], Pos) -> pos(Needle, Haystack, Pos+1).

print({Needle, undefined}) -> io:format("~s is not in haystack.~n",[Needle]);
print({Needle, Pos}) -> io:format("~s at position ~p.~n",[Needle,Pos]).
