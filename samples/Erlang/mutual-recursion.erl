-module(mutrec).
-export([mutrec/0, f/1, m/1]).

f(0) -> 1;
f(N) -> N - m(f(N-1)).

m(0) -> 0;
m(N) -> N - f(m(N-1)).

mutrec() -> lists:map(fun(X) -> io:format("~w ", [f(X)]) end, lists:seq(0,19)),
	    io:format("~n", []),
	    lists:map(fun(X) -> io:format("~w ", [m(X)]) end, lists:seq(0,19)),
	    io:format("~n", []).
