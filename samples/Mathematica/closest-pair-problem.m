nearestPair[data_] :=
 Block[{pos, dist = N[Outer[EuclideanDistance, data, data, 1]]},
  pos = Position[dist, Min[DeleteCases[Flatten[dist], 0.]]];
  data[[pos[[1]]]]]
