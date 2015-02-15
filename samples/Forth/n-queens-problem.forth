variable solutions
variable nodes

: bits ( n -- mask ) 1 swap lshift 1- ;
: lowBit  ( mask -- bit ) dup negate and ;
: lowBit- ( mask -- bits ) dup 1- and ;

: next3 ( dl dr f files -- dl dr f dl' dr' f' )
  invert >r
  2 pick r@ and 2* 1+
  2 pick r@ and 2/
  2 pick r> and ;

: try ( dl dr f -- )
  dup if
    1 nodes +!
    dup 2over and and
    begin ?dup while
      dup >r lowBit next3 recurse r> lowBit-
    repeat
  else 1 solutions +! then
  drop 2drop ;

: queens ( n -- )
  0 solutions ! 0 nodes !
  -1 -1 rot bits try
  solutions @ . ." solutions, " nodes @ . ." nodes" ;

8 queens  \ 92 solutions, 1965 nodes
