Hanoi[0, from_, to_, via_] := Null
Hanoi[n_Integer, from_, to_, via_] :=
  (Hanoi[n-1, from, via, to];
   Print["Move disk from pole ", from, " to ", to, "."];
   Hanoi[n-1, via, from, to])
