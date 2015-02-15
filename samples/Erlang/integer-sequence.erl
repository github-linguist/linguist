 F = fun(FF, I) -> io:format("~p~n", [I]), FF(FF, I + 1) end, F(F,0).
