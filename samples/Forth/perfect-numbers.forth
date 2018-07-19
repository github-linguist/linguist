: perfect? ( n -- ? )
  1
  over 2/ 1+ 2 ?do
    over i mod 0= if i + then
  loop
  = ;
