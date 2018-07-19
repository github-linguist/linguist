move(1, F, T, _V) ->
  io:format("Move from ~p to ~p~n", [F, T]);
move(N, F, T, V) ->
  move(N-1, F, V, T),
  move(1  , F, T, V),
  move(N-1, V, T, F).
