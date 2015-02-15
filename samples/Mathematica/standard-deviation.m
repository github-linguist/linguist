runningSTDDev[n_] := (If[Not[ValueQ[$Data]], $Data = {}];
  StandardDeviation[AppendTo[$Data, n]])
