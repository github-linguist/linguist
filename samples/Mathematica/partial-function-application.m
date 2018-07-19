fs[f_, s_] := Map[f, s]
f1 [n_] := n*2
f2 [n_] := n^2
fsf1[s_] := fs[f1, s]
fsf2[s_] := fs[f2, s]
