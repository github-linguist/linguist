n = 2;
While[n < 100,
 Print[Row[Riffle[Flatten[Map[Apply[ConstantArray, #] &, FactorInteger[n]]],"*"]]];
 n++]
