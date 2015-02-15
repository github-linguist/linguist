zeckendorf[0] = 0;
zeckendorf[n_Integer] :=
  10^(# - 1) + zeckendorf[n - Fibonacci[# + 1]] &@
   LengthWhile[
    Fibonacci /@
     Range[2, Ceiling@Log[GoldenRatio, n Sqrt@5]], # <= n &];
zeckendorf /@ Range[0, 20]
