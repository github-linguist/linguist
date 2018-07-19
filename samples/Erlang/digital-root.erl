-module( digital_root ).

-export( [task/0] ).

task() ->
    Ns = [N || N <- [627615, 39390, 588225, 393900588225]],
    Persistances = [persistance_root(X) || X <-	Ns],
    [io:fwrite("~p has additive persistence ~p and digital root of ~p~n", [X, Y, Z]) || {X, {Y, Z}} <- lists:zip(Ns, Persistances)].



persistance_root( X ) -> persistance_root( sum_digits:sum_digits(X), 1 ).

persistance_root( X, N ) when X	< 10 ->	{N, X};
persistance_root( X, N ) -> persistance_root( sum_digits:sum_digits(X),	N + 1 ).
