: >snumber ( str len -- 'str 'len n )
  0. 2swap
  over c@ [char] - = if
    1 /string
    >number 2swap drop
    negate
  else
    >number 2swap drop
  then ;

: expand ( str len -- )
  begin dup while
    >snumber >r
    dup if over c@ [char] - = if
      1 /string
      >snumber r> over >r
      do i . loop
    then then
    dup if over c@ [char] , = if
      1 /string
    then then
    r> .
  repeat 2drop ;

s" -6,-3--1,3-5,7-11,14,15,17-20" expand
