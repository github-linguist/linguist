Horner[l_List, x_] := Fold[x #1 + #2 &, 0, l]
Horner[{6, -4, 7, -19}, x]
-> -19 + x (7 + x (-4 + 6 x))

-19 + x (7 + x (-4 + 6 x)) /. x -> 3
-> 128
