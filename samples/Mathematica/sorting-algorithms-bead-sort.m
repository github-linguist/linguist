beadsort[ a ] := Module[ { m, sorted, s ,t },

sorted = a; m = Max[a]; t=ConstantArray[0, {m,m} ];
If[ Min[a] < 0, Print["can't sort"]];
For[ i = 1, i < Length[a], i++,  t[[i,1;;a[[i]]]]=1 ]

For[ i = 1 ,i <= m, i++, s = Total[t[[;;,i]]];
t[[ ;; , i]] = 0; t[[1 ;; s , i]] = 1; ]

For[ i=1,i<=Length[a],i++, sorted[[i]] = Total[t[[i,;;]]]; ]
Print[sorted];
]
