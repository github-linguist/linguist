combinations[n_Integer, m_Integer]/;m>= 0:=Union[Sort /@ Permutations[Range[0, n - 1], {m}]]
