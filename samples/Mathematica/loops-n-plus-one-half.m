i = 1; s = "";
While[True,
 s = s <> ToString@i;
 If[i == 10, Break[]];
 s = s <> ",";
 i++;
 ]
s
