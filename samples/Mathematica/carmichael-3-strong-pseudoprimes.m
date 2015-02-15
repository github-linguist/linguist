Cases[Cases[
  Cases[Table[{p1, h3, d}, {p1, Array[Prime, PrimePi@61]}, {h3, 2,
     p1 - 1}, {d, 1, h3 + p1 - 1}], {p1_Integer, h3_, d_} /;
     PrimeQ[1 + (p1 - 1) (h3 + p1)/d] &&
      Divisible[p1^2 + d, h3] :> {p1, 1 + (p1 - 1) (h3 + p1)/d, h3},
   Infinity], {p1_, p2_, h3_} /; PrimeQ[1 + Floor[p1 p2/h3]] :> {p1,
    p2, 1 + Floor[p1 p2/h3]}], {p1_, p2_, p3_} /;
   Mod[p2 p3, p1 - 1] == 1 :> Print[p1, "*", p2, "*", p3]]
