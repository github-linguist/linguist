$Epilog := Print["Another instance is running "];
If[Attributes[Global`Mutex] == {Protected},
 Exit[],
 Global`Mutex[x_] := Locked; Protect[Global`Mutex];
 ]
