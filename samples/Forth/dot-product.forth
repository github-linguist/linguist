: vector create cells allot ;
: th cells + ;

3 constant /vector
/vector vector a
/vector vector b

: dotproduct                           ( a1 a2 -- n)
  0 tuck ?do -rot over i th @ over i th @ * >r rot r> + loop nip nip
;

: vector! cells over + swap ?do i ! 1 cells +loop ;

-5  3 1 a /vector vector!
-1 -2 4 b /vector vector!

a b /vector dotproduct . 3 ok
