hasSmallestFactor[data_List]:=Sort[Transpose[{ParallelTable[FactorInteger[x][[1, 1]], {x, data}],data}]][[1, 2]]
