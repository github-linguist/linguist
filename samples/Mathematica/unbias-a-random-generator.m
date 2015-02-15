rand[bias_, n_] := 1 - Unitize@RandomInteger[bias - 1, n]

unbiased[bias_, n_] :=
 DeleteCases[rand[bias, {n, 2}], {a_, a_}][[All, 1]]
