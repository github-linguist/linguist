Block[{start = SessionTime[], K, t0 = 0, t1, kt0, S = 0},
 K[t_] = Sin[2 Pi f t] /. f -> 0.5; kt0 = K[t0];
 While[True, t1 = SessionTime[] - start;
  S += (kt0 + (kt0 = K[t1])) (t1 - t0)/2; t0 = t1;
  If[t1 > 2, K[t_] = 0; If[t1 > 2.5, Break[]]]]; S]
