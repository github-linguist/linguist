DynamicModule[{score, players = {1, 2}, roundscore = 0,
  roll}, (score@# = 0) & /@ players;
 Panel@Dynamic@
   Column@{Grid[
      Prepend[{#, score@#} & /@ players, {"Player", "Score"}],
      Background -> {None, 2 -> Gray}], roundscore,
     If[ValueQ@roll, Row@{"Rolled ", roll}, ""],
     If[IntegerQ@roundscore,
      Row@{Button["Roll", roll = RandomInteger[{1, 6}];
         If[roll == 1, roundscore = 0; players = RotateLeft@players,
          roundscore += roll]],
        Button["Hold", score[players[[1]]] += roundscore;
         roundscore = 0;
         If[score[players[[1]]] >= 100, roll =.;
          roundscore = Row@{players[[1]], " wins."},
          players = RotateLeft@players]]},
      Button["Play again.",
       roundscore = 0; (score@# = 0) & /@ players]]}]
