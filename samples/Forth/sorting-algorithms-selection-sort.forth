defer less?   ' < is less?

: least ( start end -- least )
  over cell+ do
    i @ over @ less? if drop i then
  cell +loop ;
: selection ( array len -- )
  cells over + tuck ( end start end )
  cell- swap do   ( end )
    i over least ( end least )
    i @ over @ i ! swap !
  cell +loop drop ;

create array 8 , 1 , 4 , 2 , 10 , 3 , 7 , 9 , 6 , 5 ,

array 10 selection
array 10 cells dump
