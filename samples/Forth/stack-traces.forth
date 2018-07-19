[UNDEFINED] R.S [IF]
\ Return stack counterpart of DEPTH
\ Note the STACK-CELLS correction is there to hide RDEPTH itself
                                       ( -- n)
: RDEPTH STACK-CELLS -2 [+] CELLS RP@ - ;

\ Return stack counterpart of .S
\ Note the : R.S R> .. >R ; sequence is there to hide R.S itself
                                       ( --)
: R.S R> CR RDEPTH DUP 0> IF DUP
  BEGIN DUP WHILE R> -ROT 1- REPEAT DROP DUP
  BEGIN DUP WHILE ROT DUP . >R 1- REPEAT DROP
  THEN ." (TORS) " DROP >R ;
[THEN]
