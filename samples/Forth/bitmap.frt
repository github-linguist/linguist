\ Bit arrays.
: bits ( u1 -- u2 ) 7 + 3 rshift ;
: bitmap ( u "name" -- ) create bits here over erase allot
   does> ( u -- a x ) over 3 rshift +  1 rot 7 and lshift ;
: bit@ ( a x -- f ) swap c@ and ;
: 1bit ( a x -- ) over c@ or swap c! ;
: 0bit ( a x -- ) invert over c@ and swap c! ;
: bit! ( f a x -- ) rot if 1bit else 0bit then ;
