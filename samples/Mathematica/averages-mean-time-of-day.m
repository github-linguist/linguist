meanTime[list_] :=
  StringJoin@
   Riffle[ToString /@
       Floor@{Mod[24 #, 24], Mod[24*60 #, 60], Mod[24*60*60 #, 60]} &[
     Arg[Mean[
        Exp[FromDigits[ToExpression@StringSplit[#, ":"], 60] & /@
            list/(24*60*60) 2 Pi I]]]/(2 Pi)], ":"];
meanTime[{"23:00:17", "23:40:20", "00:12:45", "00:17:19"}]
