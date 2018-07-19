:noname   dup . ." bottles" ;
:noname       ." 1 bottle"  ;
:noname ." no more bottles" ;
create bottles , , ,

: .bottles  dup 2 min cells bottles + @ execute ;
: .beer     .bottles ."  of beer" ;
: .wall     .beer ."  on the wall" ;
: .take     ." Take one down, pass it around" ;
: .verse    .wall cr .beer cr
         1- .take cr .wall cr ;
: verses    begin cr .verse ?dup 0= until ;

99 verses
