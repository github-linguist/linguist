-module( quicksort ).

-export( [qsort/1] ).

qsort([]) -> [];
qsort([X|Xs]) ->
   qsort([ Y || Y <- Xs, Y < X]) ++ [X] ++ qsort([ Y || Y <- Xs, Y >= X]).
