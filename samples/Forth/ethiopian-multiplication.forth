: even? ( n -- ? ) 1 and 0= ;
: e* ( x y -- x*y )
  dup 0= if nip exit then
  over 2* over 2/ recurse
  swap even? if nip else + then ;
