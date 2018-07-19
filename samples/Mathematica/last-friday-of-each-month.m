FridaysOfYear[Y_] :=
 NestWhile[(DaysPlus[#, - 1]) &, #, (DateString[#, "DayName"] != "Friday") &] & /@
  Most@Reverse@NestList [DaysPlus[# /. {x_, y_, X_} -> {x, y, 1}, - 1] &, {Y + 1, 1, 1}, 12]
Column@FridaysOfYear[2012]
