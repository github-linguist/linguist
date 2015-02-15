a[1] := 1; a[2] := 1;
a[n_] := a[n] = a[a[n-1]] + a[n-a[n-1]]

Map[Print["Max value: ",Max[Table[a[n]/n//N,{n,2^#,2^(#+1)}]]," for n between 2^",#," and 2^",(#+1)]& , Range[19]]
n=2^20; While[(a[n]/n//N)<0.55,n--]; Print["Mallows number: ",n]
