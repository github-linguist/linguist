middleThree[n_Integer] :=
 Block[{digits = IntegerDigits[n], len},
  len = Length[digits];
  If[len < 3 || EvenQ[len], "number digits odd or less than 3",
   len = Ceiling[len/2];
   StringJoin @@ (ToString /@ digits[[len - 1 ;; len + 1]])]]

testData = {123, 12345, 1234567, 987654321, 10001, -10001, -123, -100,
    100, -12345, 1, 2, -1, -10, 2002, -2002, 0};

Column[middleThree /@ testData]
