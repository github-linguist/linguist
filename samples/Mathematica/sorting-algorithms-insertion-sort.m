insertionSort[a_List] := Module[{A = a},
  For[i = 2, i <= Length[A], i++,
   value = A[[i]];    j = i - 1;
   While[j >= 1 && A[[j]] > value, A[[j + 1]] = A[[j]]; j--;];
   A[[j + 1]] = value;];
A
]
