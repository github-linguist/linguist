is_perfect(X) ->
    X == lists:sum([N || N <- lists:seq(1,X-1), X rem N == 0]).
