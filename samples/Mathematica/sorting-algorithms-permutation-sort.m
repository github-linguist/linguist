PermutationSort[x_List] := NestWhile[RandomSample, x, Not[OrderedQ[#]] &]
