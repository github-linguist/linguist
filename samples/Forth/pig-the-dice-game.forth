include lib/choose.4th
include lib/yesorno.4th

: turn                                 ( n1 -- n2)
  ." Player " . ." is up" cr           \ which player is up
  0 begin                              \ nothing so far
    s" Rolling" yes/no?                \ stand or roll?
  while                                \ now roll the dice
    6 choose 1+ dup ." Rolling " . dup 1 =
    if drop drop 0 else + ." (" dup 0 .r ." )" then cr dup 0=
  until                                \ until player stands or 1 is rolled
;

: pigthedice                           ( --)
  2 0 1 over                           \ setup players
  begin over turn + dup ." Total score: " . cr cr dup 100 < while 2swap repeat
  ." Player " swap . ." won with " . ." points." cr
  ." Player " swap . ." lost with " . ." points." cr
;                                      \ show the results

pigthedice
