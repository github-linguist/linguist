"<=: " <> ToString[#1] <> " " <> ToString[100. #1/#2] <> "%\n >: " <>
     ToString[#2 - #1] <> " " <> ToString[100. (1 - #1/#2)] <> "%" &[
   Count[Total /@ Subsets[Join[#1, #2], {Length@#1}],
    n_ /; n <= Total@#1],
   Binomial[Length@#1 + Length@#2, Length@#1]] &[{85, 88, 75, 66, 25,
  29, 83, 39, 97}, {68, 41, 10, 49, 16, 65, 32, 92, 28, 98}]
