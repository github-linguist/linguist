1 31 lshift 1- constant MAX-RAND-BSD
1 15 lshift 1- constant MAX-RAND-MS

variable seed                         \ seed variable

: (random) seed @ * + dup seed ! ;    ( -- n)
: BSDrandom MAX-RAND-BSD 12345 1103515245 (random) and ;
: MSrandom MAX-RAND-MS 2531011 214013 (random) 16 rshift and ;

: test-random
  1 seed ! cr ." BSD (seed=1)" cr
  5 0 do BSDrandom . cr loop
  1 seed ! cr ." MS  (seed=1)" cr
  5 0 do MSrandom . cr loop
;

test-random
