tmp = "";
For[i = 1, i <= 10, i++,
  tmp = tmp <> ToString[i];
  If[Mod[i, 5] == 0,
   tmp = tmp <> "\n";
   ,
   tmp = tmp <> ", ";
   ];
  ];
Print[tmp]
