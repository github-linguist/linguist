DynamicModule[{data =
   ArrayPad[PadRight[Characters /@ StringSplit["tH.........
        .   .
           ...
        .   .
        Ht.. ......", "\n"]] /. {" " -> 0, "t" -> 2, "H" -> 1,
      "." -> 3}, 1]},
 Dynamic@ArrayPlot[
   data = CellularAutomaton[{{{_, _, _}, {_, 0, _}, {_, _, _}} ->
       0, {{_, _, _}, {_, 1, _}, {_, _, _}} ->
       2, {{_, _, _}, {_, 2, _}, {_, _, _}} ->
       3, {{a_, b_, c_}, {d_, 3, e_}, {f_, g_, h_}} :>
       Switch[Count[{a, b, c, d, e, f, g, h}, 1], 1, 1, 2, 1, _, 3]},
     data], ColorRules -> {1 -> Yellow, 2 -> Red}]]
