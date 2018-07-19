programCount[fn_] :=  Reverse[If[Length[#] > 10, Take[#, -10], #] &[SortBy[Tally[Cases[DownValues[fn], s_Symbol, \[Infinity], Heads -> True]], Last]]]
