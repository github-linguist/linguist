rms(Nums) ->
    math:sqrt(lists:foldl(fun(E,S) -> S+E*E end, 0, Nums) / length(Nums)).

rms([1,2,3,4,5,6,7,8,9,10]).
