Eratosthenes[n_] := Module[
   {numbers = Range[n]},

   Do[If[numbers[[i]] != 0,
     Do[numbers[[i j]] = 0, {j, 2, n/i}]],
    {i, 2, Sqrt[n]}];

   Select[numbers, # > 1 &]];

Eratosthenes[100]
