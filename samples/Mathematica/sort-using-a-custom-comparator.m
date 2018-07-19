StringOrderQ[x_String, y_String] :=
 If[StringLength[x] == StringLength[y],
       OrderedQ[{x, y}],
       StringLength[x] >StringLength[y]
   ]
words={"on","sunday","sander","sifted","and","sorted","sambaa","for","a","second"};
Sort[words,StringOrderQ]
