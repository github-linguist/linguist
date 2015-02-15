LeftTruncatablePrimeQ[n_] := Times @@ IntegerDigits[n] > 0 &&
  And @@ PrimeQ /@ ToExpression /@ StringJoin /@
      Rest[Most[NestList[Rest, #, Length[#]] &[Characters[ToString[n]]]]]
RightTruncatablePrimeQ[n_] := Times @@ IntegerDigits[n] > 0 &&
  And @@ PrimeQ /@ ToExpression /@ StringJoin /@
      Rest[Most[NestList[Most, #, Length[#]] &[Characters[ToString[n]]]]]
