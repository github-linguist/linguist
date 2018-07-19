DynamicModule[{record, play, text = "\nRock-paper-scissors\n",
  choices = {"Rock", "Paper", "Scissors"}},
 Evaluate[record /@ choices] = {1, 1, 1};
 play[x_] :=
  Module[{y = RandomChoice[record /@ choices -> RotateLeft@choices]},
   record[x]++;
   text = "Your Choice:" <> x <> "\nComputer's Choice:" <> y <> "\n" <>
      Switch[{x, y}, Alternatives @@ Partition[choices, 2, 1, 1],
      "You lost.",
      Alternatives @@ Reverse /@ Partition[choices, 2, 1, 1],
      "You win.", _, "Draw."]];
 Column@{Dynamic[text], ButtonBar[# :> play[#] & /@ choices]}]
