hq9plus[program_] :=
 Module[{accumulator = 0, bottle},
  bottle[n_] :=
   ToString[n] <> If[n == 1, " bottle", " bottles"] <> " of beer";
  Do[Switch[chr, "H", Print@"hello, world", "Q", Print@program, "9",
    Print@StringJoin[
      Table[bottle[n] <> " on the wall\n" <> bottle[n] <>
        "\ntake one down, pass it around\n" <> bottle[n - 1] <>
        " on the wall" <> If[n == 1, "", "\n\n"], {n, 99, 1, -1}]],
    "+", accumulator++], {chr, Characters@program}]; accumulator]
