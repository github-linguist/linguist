guessnumber[min_, max_] :=
 Module[{number = RandomInteger[{min, max}], guess},
  While[guess =!= number,
   guess = Input[
     If[guess > number, "Too high.Guess again.",
      "Too low.Guess again.",
      "Guess a number between " <> ToString@min <> " and " <>
       ToString@max <> "."]]];
  CreateDialog[{"Well guessed!", DefaultButton[]}]];
guessnumber[1, 10]
