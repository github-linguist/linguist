: forward-difference
  dup 0
  ?do
     swap rot over i 1+ - 0
     ?do dup i cells + dup cell+ @ over @ - swap ! loop
     swap rot
  loop -
;

create a
  90 , 47 , 58 , 29 , 22 , 32 , 55 , 5 , 55 , 73 ,

: test a 10 9 forward-difference bounds ?do i ? loop ;

test
