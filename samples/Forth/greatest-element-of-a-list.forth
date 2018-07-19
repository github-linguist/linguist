: array-max ( addr len -- max )
  dup 0= if nip exit then
  over @  rot cell+  rot 1-
  cells bounds ?do  i @ max  cell +loop ;

: stack-max ( n ... m count -- max ) 1 ?do max loop ;
