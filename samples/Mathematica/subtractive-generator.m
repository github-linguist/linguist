initialize[n_] :=
 Module[{buffer},
  buffer =
   Join[Nest[Flatten@{#, Mod[Subtract @@ #[[-2 ;;]], 10^9]} &, {n, 1},
       53][[1 + Mod[34 Range@54, 55]]], {n}];
  Nest[nextValue, buffer, 165]]

  nextValue[buffer_] :=
 Flatten@{Rest@buffer, Mod[Subtract @@ buffer[[{1, 32}]], 10^9]}
