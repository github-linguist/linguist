IsPrime[n_Integer] :=
 Module[{k = 2},
  If[n <= 1, Return False];
  If[n == 2, Return True];
  While[k <= Sqrt[n],
   If[Mod[n, k] == 0, Return[False], k++]
   ];
  Return[True]
 ]
