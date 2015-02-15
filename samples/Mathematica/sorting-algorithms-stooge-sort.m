stoogeSort[lst_, I_, J_] := Module[{i = I, j = J, list = lst},

 If[list[[j]] < list[[i]], list[[{i,j}]] = list[[{j,i}]];]

 If[(j-i) > 1, t = Round[(j-i+1)/3];
  list=stoogeSort[list,i,j-t];
  list=stoogeSort[list,i+t,j];
  list=stoogeSort[list,i,j-t];];

 list
]
