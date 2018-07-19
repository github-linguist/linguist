seq[n_, b_] := FixedPointList[Total[IntegerDigits[#, b]] &, n];
root[n_Integer, base_: 10] := If[base == 10, #, BaseForm[#, base]] &[Last[seq[n, base]]]
persistance[n_Integer, base_: 10] := Length[seq[n, base]] - 2;
