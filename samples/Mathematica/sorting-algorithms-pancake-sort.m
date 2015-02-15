LMaxPosition[ a_, n_ ] := Part[Position[a[[;;n]],Max[a[[;;n]]]],1,1]

SetAttributes[Flip,HoldFirst]; Flip[a_] := Set[a,Reverse[a]]

pancakeSort[a_] : = For[n = Length[a], n > 1, n--,
 If[LMaxPosition[a,n] < n,
  Flip[a[[;;LMaxPosition[a,n]]]]; Print[a];
  Flip[a[[;;n]]]; Print[a];
 ];
];
