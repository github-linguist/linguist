choose(N, K) when is_integer(N), is_integer(K), (N >= 0), (K >= 0), (N >= K) ->
  choose(N, K, 1, 1).

choose(N, K, K, Acc) ->
  (Acc * (N-K+1)) div K;
choose(N, K, I, Acc) ->
  choose(N, K, I+1, (Acc * (N-I+1)) div I).
