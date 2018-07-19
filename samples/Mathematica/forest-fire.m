evolve[nbhd_List, k_] := 0 /; nbhd[[2, 2]] == 2    (*burning->empty*)
evolve[nbhd_List, k_] := 2 /; nbhd[[2, 2]] == 1 && Max@nbhd == 2     (*near_burning&nonempty->burning*)
evolve[nbhd_List, k_] := RandomChoice[{f, 1 - f} -> {2, nbhd[[2, 2]]}] /; nbhd[[2, 2]] == 1 && Max@nbhd < 2   (*spontaneously combusting tree*)
evolve[nbhd_List, k_] := RandomChoice[{p, 1 - p} -> {1, nbhd[[2, 2]]}] /; nbhd[[2, 2]] == 0  (*random tree growth*)

r = 100; c = 100; p = 10^-2; f = 10^-4;
init = RandomInteger[BernoulliDistribution[0.05], {r, c}];
MatrixPlot[CellularAutomaton[{evolve, {}, {1, 1}}, {init, 0}, {{{300}}}], ColorRules -> {0 -> White, 1 -> Green, 2 -> Red}, Frame -> False]
