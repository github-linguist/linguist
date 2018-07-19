create values
here
    0 ,  1 ,  2 ,  4 ,  6 ,  7 ,  8 , 11 , 12 , 14 ,
   15 , 16 , 17 , 18 , 19 , 20 , 21 , 22 , 23 , 24 ,
   25 , 27 , 28 , 29 , 30 , 31 , 32 , 33 , 35 , 36 ,
   37 , 38 , 39 ,
here swap - 1 cells / constant /values

: clip 1- swap cell+ swap ;            \ reduce array
: .range2 0 .r ." -" 0 .r ;            \ difference two or more
: .range1 0 .r ." , " 0 .r ;           \ difference one
: .range0 drop 0 .r ;                  \ no difference
                                       \ select printing routine
create .range ' .range0 , ' .range1 , ' .range2 ,
  does> >r over over - 2 min cells r> + @ execute ;

: .ranges                              ( a n --)
  over @ dup >r >r                     \ setup first value
  begin
    clip dup                           \ check length array
  while
    over @ dup r@ 1+ =                 \ check if range breaks
    if r> drop >r else r> r> .range ." , " dup >r >r then
  repeat 2drop r> r> .range cr         \ print last range
;

values /values .ranges
