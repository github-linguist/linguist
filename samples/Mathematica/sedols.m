SEDOL[Code_?(Function[v,StringFreeQ[v,{"A","E","I","O","U"}]])]:=
Code<>ToString[10-Mod[ToExpression[Quiet[Flatten[Characters[Code]
/.x_?LetterQ->(ToCharacterCode[x]-55)]]].{1,3,1,7,3,9},10]]

Scan[Print[SEDOL[#]] &, {"710889","B0YBKJ","406566","B0YBLH","228276","B0YBKL","557910","B0YBKR","585284","B0YBKT","B00030","DUMMY"}]

->Output:
7108899
B0YBKJ7
4065663
B0YBLH2
2282765
B0YBKL9
5579107
B0YBKR5
5852842
B0YBKT7
B0003010
SEDOL[DUMMY] -> rejected
