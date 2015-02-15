IntegerHalving[x_]:=Floor[x/2]
IntegerDoubling[x_]:=x*2;
OddInteger           OddQ
Ethiopian[x_, y_] :=
Total[Select[NestWhileList[{IntegerHalving[#[[1]]],IntegerDoubling[#[[2]]]}&, {x,y}, (#[[1]]>1&)], OddQ[#[[1]]]&]][[2]]

Ethiopian[17, 34]
