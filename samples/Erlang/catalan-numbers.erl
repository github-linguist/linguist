-module(catalan).

-export([test/0]).

cat(N) ->
   factorial(2 * N) div (factorial(N+1) * factorial(N)).

factorial(N) ->
   fac1(N,1).

fac1(0,Acc) ->
   Acc;
fac1(N,Acc) ->
   fac1(N-1, N * Acc).

cat_r1(0) ->
   1;
cat_r1(N) ->
   lists:sum([cat_r1(I)*cat_r1(N-1-I) || I <- lists:seq(0,N-1)]).

cat_r2(0) ->
   1;
cat_r2(N) ->
   cat_r2(N - 1) * (2 * ((2 * N) - 1)) div (N + 1).

test() ->
    TestList = lists:seq(0,14),
    io:format("Directly:\n~p\n",[[cat(N) || N <- TestList]]),
    io:format("1st recusive method:\n~p\n",[[cat_r1(N) || N <- TestList]]),
    io:format("2nd recusive method:\n~p\n",[[cat_r2(N) || N <- TestList]]).
