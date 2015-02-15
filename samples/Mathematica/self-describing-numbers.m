isSelfDescribing[n_Integer] := (RotateRight[DigitCount[n]] == PadRight[IntegerDigits[n], 10])
