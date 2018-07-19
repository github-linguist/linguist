Do[ Print[m[[i, j]]];
    If[m[[i, j]] === 20, Return[]],
  {i, 1, Dimensions[m][[1]]},
  {j, 1, Dimensions[m][[2]]}]
