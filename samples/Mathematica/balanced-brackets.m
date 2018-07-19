(* Generate open/close events. *)
gen[n_] := RandomSample[Table[{1, -1}, {n}] // Flatten]

(* Check balance. *)
check[lst_] := And @@ (# >= 0 & /@ Accumulate[lst])

(* Do task for string with n opening and n closing brackets. *)
doString[n_] := (
  lst = gen[n];
  str = StringJoin[lst /. {1 -> "[", -1 -> "]"}];
  Print[str <> If[match[lst, 0],
     "  is balanced.",
     "  is not balanced."]])
