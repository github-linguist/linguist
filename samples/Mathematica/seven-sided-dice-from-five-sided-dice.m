sevenFrom5Dice := (tmp$ = 5*RandomInteger[{1, 5}] + RandomInteger[{1, 5}] - 6;
  If [tmp$ < 21, 1 + Mod[tmp$ , 7], sevenFrom5Dice])
