DynamicModule[{board = ConstantArray[0, {3, 3}], text = "Playing...",
  first, rows =
   Join[#, Transpose@#, {Diagonal@#, Diagonal@Reverse@#}] &},
 Column@{Graphics[{Thickness[.02],
     Table[With[{i = i, j = j},
       Button[{White, Rectangle[{i, j} - 1, {i, j}], Black,
         Dynamic[Switch[board[[i, j]], 0, Black, 1,
           Circle[{i, j} - .5, .3], -1,
           Line[{{{i, j} - .2, {i, j} - .8}, {{i - .2,
               j - .8}, {i - .8, j - .2}}}]]]},
        Which[text != "Playing...", board = ConstantArray[0, {3, 3}];
         text = "Playing...", board[[i, j]] == 0,
         If[board == ConstantArray[0, {3, 3}],
          first = {i,
             j} /. {{2, 2} -> 1, {1 | 3, 1 | 3} -> 2, _ -> 3}];
         board[[i, j]] = 1;
         FinishDynamic[];
         Which[MemberQ[rows[board], {1, 1, 1}], text = "You win.",
          FreeQ[board, 0], text = "Draw.", True,
          board[[Sequence @@
              SortBy[Select[Tuples[{Range@3, Range@3}],
                 board[[Sequence @@ #]] ==
                   0 &], -Total[
                    Sort /@
                    rows[ReplacePart[
                    board, # -> -1]] /. {{-1, -1, -1} ->
                    512, {-1, 1, 1} -> 64, {-1, -1, 0} ->
                    8, {0, 1, 1} -> -1, {_, _, _} -> 0}] -
                  Switch[#, {2, 2}, 1, {1 | 3, 1 | 3},
                   If[first == 2, -1, 0], _,
                   If[first == 2, 0, -1]] &][[1]]]] = -1;
          Which[MemberQ[rows[board], {-1, -1, -1}],
           text = "You lost.", FreeQ[board, 0],
           text = "Draw."]]]]], {i, 1, 3}, {j, 1, 3}], Thickness[.01],
      Line[{{{1, 0}, {1, 3}}, {{2, 0}, {2, 3}}, {{0, 1}, {3, 1}}, {{0,
          2}, {3, 2}}}]}], Dynamic@text}]
