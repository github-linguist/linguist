binary(X) :- format('~2r~n', [X]).
main :- maplist(binary, [5,50,9000]), halt.
