guessnumber[min0_, max0_] :=
  DynamicModule[{min = min0, max = max0, guess, correct = False},
   guess[] := Round@Mean@{min, max};
   Dynamic@If[correct, Row@{"Your number is ", guess[], "."},
     Column@{Row@{"I guess ", guess[], "."},
       Row@{Button["too high", max = guess[]],
         Button["too low", min = guess[]],
         Button["correct", correct = True]}}]];
guessnumber[1, 100]
