len[n_] := RealDigits[n][[2]]; padding = len[Max@ Quotient[inputdata, 10]];

For[i = Min@ Quotient[inputdata, 10],i <= Max@ Quotient[inputdata, 10], i++,
 (Print[i, If[(padding - len[i]) > 0, (padding - len[i])*" " <> " |", " |"] ,
 StringJoin[(" " <> #) & /@ Map[ToString, #]]])&@
  Select[{Quotient[#, 10], Mod[#, 10]} & /@ Sort[inputdata],Part[#, 1] == i &][[;; , 2]]]
