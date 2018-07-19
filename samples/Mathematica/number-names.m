small = "zero"["one", "two", "three", "four", "five", "six", "seven",
  "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
  "fifteen", "sixteen", "seventeen", "eighteen",
  "nineteen"]; tens = # <> "-" & /@ {"twenty", "thirty", "forty",
   "fifty", "sixty", "seventy", "eighty", "ninety"};
big = Prepend[
   " " <> # & /@ {"thousand", "million", "billion", "trillion",
     "quadrillion", "quintillion", "sextillion", "septillion",
     "octillion", "nonillion", "decillion", "undecillion",
     "duodecillion", "tredecillion"}, ""];
name[n_Integer] := "negative " <> name[-n] /; n < 0;
name[n_Integer] := small[[n]] /; 0 <= n < 20;
name[n_Integer] :=
  StringTrim[tens[[#1 - 1]] <> small[[#2]] & @@ IntegerDigits[n],
    "-zero"] /; 10 <= n < 100;
name[n_Integer] :=
 StringTrim[
   small[[#1]] <> " hundred and " <> name@#2 & @@
    IntegerDigits[n, 100], " and zero"] /; 100 <= n < 1000;
name[n_Integer] :=
 StringJoin@
  Riffle[Select[
    MapThread[StringJoin, {name /@ #, Reverse@big[[;; Length@#]]}] &@
     IntegerDigits[n, 1000], StringFreeQ[#, "zero"] &], ","];
