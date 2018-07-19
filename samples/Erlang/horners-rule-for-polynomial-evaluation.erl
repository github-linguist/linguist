horner(L,X) ->
  lists:foldl(fun(C, Acc) -> X*Acc+C end,0, lists:reverse(L)).
t() ->
  horner([-19,7,-4,6], 3).
