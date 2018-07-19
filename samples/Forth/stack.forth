: stack ( size -- )
  create here cell+ ,  cells allot ;

: push ( n st -- ) tuck @ !  cell swap +! ;
: pop ( st -- n ) -cell over +!  @ @ ;
: empty? ( st -- ? ) dup @ - cell+ 0= ;

10 stack st

1 st push
2 st push
3 st push
st empty? .  \ 0 (false)
st pop . st pop . st pop .  \ 3 2 1
st empty? .  \ -1 (true)
