deconv[f_List, g_List] :=
 Module[{A =
    SparseArray[
     Table[Band[{n, 1}] -> f[[n]], {n, 1, Length[f]}], {Length[g], Length[f] - 1}]},
  Take[LinearSolve[A, g], Length[g] - Length[f] + 1]]
