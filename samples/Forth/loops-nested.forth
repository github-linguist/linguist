include random.fs

10 constant X
10 constant Y

: ,randoms ( range n -- ) 0 do dup random 1+ , loop drop ;

create 2darray 20 X Y * ,randoms

: main
  Y 0 do
    cr
    X 0 do
      j X * i + cells 2darray + @
      dup .
      20 = if unloop unloop exit then
    loop
  loop ;
