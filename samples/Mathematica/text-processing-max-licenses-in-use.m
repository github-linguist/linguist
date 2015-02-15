LC = 0; LCMax = 0; Scan[
   If[MemberQ[#, "OUT"], LC++;
      If[LCMax < LC, LCMax = LC; LCMaxtimes = {};];
      If[LCMax == LC, AppendTo[LCMaxtimes, #[[4]]]],
   LC--;] &, Import["mlijobs.txt", "Table"]]
Print["The maximum number of licenses used was ", LCMax, ", at ", LCMaxtimes]
