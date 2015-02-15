: third ( a b c -- a b c a ) 2 pick ;
: reduce ( xt n addr cnt -- n' ) \ where xt ( a b -- n )
  cells bounds do i @ third execute  cell +loop nip ;

create a 1 , 2 , 3 , 4 , 5 ,

' + 0 a 5 reduce .    \ 15
' * 1 a 5 reduce .    \ 120
