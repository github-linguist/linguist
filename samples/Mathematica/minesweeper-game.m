DynamicModule[{m = 6, n = 4, minecount, grid, win, reset, clear,
  checkwin},
 reset[] :=
  Module[{minesdata, adjacentmines},
   minecount = RandomInteger[Round[{.1, .2} m*n]];
   minesdata =
    Normal@SparseArray[# -> 1 & /@
       RandomSample[Tuples[Range /@ {m, n}], minecount], {m, n}];
   adjacentmines =
    ArrayPad[
     Total[RotateLeft[
         ArrayPad[minesdata,
          1], #] & /@ {{-1, -1}, {0, -1}, {1, -1}, {-1, 0}, {1,
         0}, {-1, 1}, {0, 1}, {1, 1}}], -1];
   grid = Array[{If[minesdata[[#1, #2]] == 1, "*",
        adjacentmines[[#1, #2]]], ".", 1} &, {m, n}]; win = ""];
 clear[i_, j_] :=
  If[grid[[i, j, 1]] == "*", win = "You lost.";
    grid = grid /. {{n_Integer, "?", _} :> {n, "x", 0}, {"*",
         ".", _} :> {"*", "*", 0}},
    grid[[i, j]] = {grid[[i, j, 1]], grid[[i, j, 1]], 0};
    If[grid[[i, j, 2]] == 0, grid[[i, j, 2]] = "";
     clear[i + #[[1]],
        j + #[[2]]] & /@ {{-1, -1}, {0, -1}, {1, -1}, {-1, 0}, {1,
        0}, {-1, 1}, {0, 1}, {1, 1}}]] /;
   1 <= i <= m && 1 <= j <= n && grid[[i, j, 2]] == ".";
 checkwin[] :=
  If[FreeQ[grid, {_Integer, "?", _} | {_, "*", _} | {_Integer,
      ".", _}], win = "You win.";
   grid = grid /. {{"*", ".", _} :> {"*", "?", 1}}];
 reset[];
 Panel@Column@{Row@{Dynamic@minecount, "\t",
      Button["Reset", reset[]]},
    Grid[Table[
      With[{i = i, j = j},
       EventHandler[
        Button[Dynamic[grid[[i, j, 2]]], Null, ImageSize -> {17, 17},
         Appearance ->
          Dynamic[If[grid[[i, j, 3]] == 0, "Pressed",
            "DialogBox"]]], {{"MouseClicked", 1} :>
          If[win == "", clear[i, j]; checkwin[]], {"MouseClicked",
           2} :> If[win == "",
           Switch[grid[[i, j, 2]], ".", grid[[i, j, 2]] = "?";
            minecount--, "?", grid[[i, j, 2]] = "."; minecount++];
           checkwin[]]}]], {i, 1, m}, {j, 1, n}], Spacings -> {0, 0}],
     Dynamic[win]}]
