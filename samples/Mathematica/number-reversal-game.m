Module[{array = Range@9, score = 0},
 While[array == Range@9, array = RandomSample@Range@9];
 While[array != Range@9,
  Print@array; (array[[;; #]] = Reverse@array[[;; #]]) &@
   Input["How many digits would you like to reverse?"]; score++];
 Print@array; Print["Your score:", score]]
