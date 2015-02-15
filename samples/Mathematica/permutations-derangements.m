Needs["Combinatorica`"]
derangements[n_] := Derangements[Range[n]]
derangements[4]
Table[{NumberOfDerangements[i], Subfactorial[i]}, {i, 9}] // TableForm
Subfactorial[20]
