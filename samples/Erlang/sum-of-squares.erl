lists:foldl(fun(X, Sum) -> X*X + Sum end, 0, [3,1,4,1,5,9]).
