textMenu[data_List] := (MapIndexed[Print[#2[[1]], ") ", #] &, {a, b, c}];
  While[! (IntegerQ[choice] && Length[data] > choice > 0),
      choice = Input["Enter selection"]];
  data[[choice]])
