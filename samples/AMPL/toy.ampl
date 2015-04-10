# A toy knapsack problem from the LocalSolver docs written in AMPL.

set I;
param Value{I};
param Weight{I};
param KnapsackBound;
var Take{I} binary;

maximize TotalValue: sum{i in I} Take[i] * Value[i];
s.t. WeightLimit: sum{i in I} Take[i] * Weight[i] <= KnapsackBound;

data;

param:
I: Weight Value :=
0    10     1
1    60    10
2    30    15
3    40    40
4    30    60
5    20    90
6    20   100
7     2    15;

param KnapsackBound := 102;
