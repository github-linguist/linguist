isLegal[n_List, x_String] :=
 Quiet[Check[
   With[{h = ToExpression[x, StandardForm, HoldForm]},
    If[Cases[Level[h, {2, \[Infinity]}, Hold, Heads -> True],
        Except[_Integer | Plus | _Plus | Times | _Times | Power |
          Power[_, -1]]] === {} &&
      Sort[Level[h /. Power[q_, -1] -> q, {-1}] /.
         q_Integer -> Abs[q]] === Sort[n], ReleaseHold[h]]], Null]]
Grid[{{Button[
    "new numbers", {a, b, c, d} = Table[RandomInteger[{1, 9}], {4}]],
   InputField[Dynamic[x], String]}, {Dynamic[{a, b, c, d}],
   Dynamic[Switch[isLegal[{a, b, c, d}, x], Null,
     "Sorry, that is invalid.", 24, "Congrats! That's 24!", _,
     "Sorry, that makes " <> ToString[ToExpression@x, InputForm] <>
      ", not 24."]]}}]
