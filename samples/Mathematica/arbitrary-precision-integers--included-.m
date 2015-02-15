s:=ToString[5^4^3^2];
Print[StringTake[s,20]<>"..."<>StringTake[s,-20]<>" ("<>ToString@StringLength@s<>" digits)"];
