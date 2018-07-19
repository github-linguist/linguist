order[List1_, List2_] := With[{
   L1 = List1[[1 ;; Min @@ Length /@ {List1, List2}]],
   L2 = List2[[1 ;; Min @@ Length /@ {List1, List2}]]
},
   If [Thread[Order[L1, L2]] == 0,
   Length[List1] < Length[List2],
   Thread[Order[L1, L2]] == 1
   ]]
