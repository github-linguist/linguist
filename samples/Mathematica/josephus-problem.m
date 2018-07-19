survivor[n_, k_] := Nest[Most[RotateLeft[#, k]] &, Range[0, n - 1], n - 1]
survivor[41, 3]
