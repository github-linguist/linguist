LuhnQ[nb_] := (Mod[Total[(2*ToExpression[#[[2;;All;;2]]]) /. {z_?(Function[v, v>9]) -> z-9}]
       + Total[ToExpression[#[[1;;All;;2]]]], 10] == 0)& [Characters[StringReverse[ToString[nb]]] ]

LuhnQ /@ {49927398716, 49927398717, 1234567812345678, 1234567812345670}
->{True, False, False, True}
