selfRefSequence[ x_ ] := FromDigits@Flatten@Reverse@Cases[Transpose@{RotateRight[DigitCount@x,1], Range[0,9]},Except[{0,_}]]
DisplaySequence[ x_ ] := NestWhileList[selfRefSequence,x,UnsameQ[##]&,4]
data= {#, Length@DisplaySequence[#]}&/@Range[1000000];
Print["Values: ", Select[data  ,#[[2]] == Max@data[[;;,2]]&][[1,;;]]]
Print["Iterations: ", Length@DisplaySequence[#]&/@Select[data  ,#[[2]] == Max@data[[;;,2]]&][[1,;;]]]
DisplaySequence@Select[data, #[[2]] == Max@data[[;;,2]]&][[1]]//Column
