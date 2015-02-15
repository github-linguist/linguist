flip[a_] :=
 Block[{a1 = First@a},
  If[a1 == Length@a, Reverse[a],
   Join[Reverse[a[[;; a1]]], a[[a1 + 1 ;;]]]]]

swaps[a_] := Length@FixedPointList[flip, a] - 2

Print[#, ": ", Max[swaps /@ Permutations[Range@#]]] & /@ Range[10];
