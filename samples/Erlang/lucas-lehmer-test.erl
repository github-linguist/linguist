-module(mp).
-export([main/0]).

main() -> [ io:format("M~p ", [P]) || P <- lists:seq(2,700), (P == 2) orelse (s((1 bsl P) - 1, P-1) == 0) ].

s(MP,1) -> 4 rem MP;
s(MP,N) -> X=s(MP,N-1), (X*X - 2) rem MP.
