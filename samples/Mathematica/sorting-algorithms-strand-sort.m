StrandSort[ input_ ] := Module[ {results = {}, A = input},
While[Length@A > 0,
 sublist = {A[[1]]}; A = A[[2;;All]];
  For[i = 1, i < Length@A, i++,
   If[ A[[i]] > Last@sublist, AppendTo[sublist, A[[i]]]; A = Delete[A, i];]
  ];
 results = #[[Ordering@#]]&@Join[sublist, results];];
results ]
