For[i = 2, i < Prime[1000000], i = NextPrime[i],
 If[Mod[2^44497, i] == 1,
  Print["divisible by "<>i]]]; Print["prime test passed; call Lucas and Lehmer"]
