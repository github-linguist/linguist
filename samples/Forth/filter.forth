: sel ( dest 0 test src len -- dest len )
  cells over + swap do   ( dest len test )
    i @ over execute if
      i @ 2over cells + !
      >r 1+ r>
    then
  cell +loop drop ;

create nums 1 , 2 , 3 , 4 , 5 , 6 ,
create evens 6 cells allot

: .array  0 ?do dup i cells + @ . loop drop ;

: even? ( n -- ? ) 1 and 0= ;

evens 0 ' even? nums 6 sel .array        \ 2 4 6
