include random.fs

: shuffle ( deck size -- )
  2 swap do
    dup i random cells +
    over @ over @  swap
    rot  ! over !
    cell+
  -1 +loop drop ;

: .array   0 do dup @ . cell+ loop drop ;

create deck 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 ,

deck 10 2dup shuffle .array
