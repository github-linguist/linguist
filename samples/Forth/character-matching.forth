: starts-with ( a l a2 l2 -- ? )
  tuck 2>r min 2r> compare 0= ;
: ends-with ( a l a2 l2 -- ? )
  tuck 2>r negate over + 0 max /string 2r> compare 0= ;
\ use SEARCH ( a l a2 l2 -- a3 l3 ? ) for contains
