fractalTree[
  pt : {_, _}, \[Theta]orient_: \[Pi]/2, \[Theta]sep_: \[Pi]/9,
  depth_Integer: 9] := Module[{pt2},
  If[depth == 0, Return[]];
  pt2 = pt + {Cos[\[Theta]orient], Sin[\[Theta]orient]}*depth;
  DeleteCases[
   Flatten@{
     Line[{pt, pt2}],
     fractalTree[pt2, \[Theta]orient - \[Theta]sep, \[Theta]sep,
      depth - 1],
     fractalTree[pt2, \[Theta]orient + \[Theta]sep, \[Theta]sep,
      depth - 1]
     },
   Null
   ]
  ]
Graphics[fractalTree[{0, 0}, \[Pi]/2, \[Pi]/9]]
