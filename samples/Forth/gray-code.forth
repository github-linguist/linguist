: >gray ( n -- n ) dup 2/ xor ;

: gray> ( n -- n )
  0  1 31 lshift  ( g b mask )
  begin
    >r
     2dup 2/ xor
     r@ and or
    r> 1 rshift
    dup 0=
  until
  drop nip ;

: test
  2 base !
  32 0 do
    cr i  dup 5 .r ."  ==> "
    >gray dup 5 .r ."  ==> "
    gray>     5 .r
  loop
  decimal ;
