: u>f  0 d>f ;
: f>u  f>d drop ;

: fn ( n -- n ) dup u>f fsqrt fround f>u + ;
: test ( n -- ) 1 do i fn . loop ;
23 test    \ 2 3 5 6 7 8 10 11 12 13 14 15 17 18 19 20 21 22 23 24 26 27  ok

: square? ( n -- ? ) u>f fsqrt  fdup fround f-  f0= ;
: test ( n -- ) 1 do i fn square? if cr i . ." fn was square" then loop ;
1000000 test    \ ok
