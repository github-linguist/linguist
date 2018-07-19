1024 constant size
create buffer size cells allot
here constant end
variable head  buffer head !
variable tail  buffer tail !
variable used       0 used !

: empty?  used @ 0= ;
: full?   used @ size = ;

: next ( ptr -- ptr )
  cell+  dup end = if drop buffer then ;

: put ( n -- )
  full? abort" buffer full"
  \ begin full? while pause repeat
  tail @ !  tail @ next tail !   1 used +! ;

: get ( -- n )
  empty? abort" buffer empty"
  \ begin empty? while pause repeat
  head @ @  head @ next head !  -1 used +! ;
