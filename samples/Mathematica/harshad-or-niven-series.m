nextHarshad =
  NestWhile[# + 1 &, # + 1, ! Divisible[#, Total@IntegerDigits@#] &] &;
Print@Rest@NestList[nextHarshad, 0, 20];
Print@nextHarshad@1000;
