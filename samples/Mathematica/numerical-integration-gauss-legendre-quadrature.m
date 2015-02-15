gaussLegendreQuadrature[func_, {a_, b_}, degree_: 5] :=
Block[{nodes, x, weights},
 nodes = Cases[NSolve[LegendreP[degree, x] == 0, x], _?NumericQ, Infinity];
 weights = 2 (1 - nodes^2)/(degree LegendreP[degree - 1, nodes])^2;
 (b - a)/2 weights.func[(b - a)/2 nodes + (b + a)/2]]

gaussLegendreQuadrature[Exp, {-3, 3}]
