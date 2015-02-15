   numbers =. 12757923 12878611 12878893 12757923 15808973 15780709 197622519
   factors =. q:&.> parallelize 2 numbers NB. q: is parallelized here
   ind =. (i. >./) <./@> factors
   ind { numbers ;"_1 factors
┌────────┬───────────┐
│12878611│47 101 2713│
└────────┴───────────┘
