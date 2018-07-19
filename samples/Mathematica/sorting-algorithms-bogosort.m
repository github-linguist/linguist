Bogosort[x_List] := Block[{t=x},While[!OrderedQ[t],t=RandomSample[x]]; t]

Bogosort[{1, 2, 6, 4, 0, -1, Pi, 3, 5}]
=> {-1, 0, 1, 2, 3, Pi, 4, 5, 6}
