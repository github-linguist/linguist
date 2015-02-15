: merge-step ( right mid left -- right mid+ left+ )
  over @ over @ < if
    over @ >r
    2dup - over dup cell+ rot move
    r> over !
    >r cell+ 2dup = if rdrop dup else r> then
  then cell+ ;
: merge ( right mid left -- right left )
  dup >r begin 2dup > while merge-step repeat 2drop r> ;

: mid ( l r -- mid ) over - 2/ cell negate and + ;

: mergesort ( right left -- right left )
  2dup cell+ <= if exit then
  swap 2dup mid recurse rot recurse merge ;

: sort ( addr len -- )  cells over + swap mergesort 2drop ;

create test 8 , 1 , 5 , 3 , 9 , 0 , 2 , 7 , 6 , 4 ,

: .array ( addr len -- ) 0 do dup i cells + @ . loop drop ;

test 10 2dup sort .array       \ 0 1 2 3 4 5 6 7 8 9
