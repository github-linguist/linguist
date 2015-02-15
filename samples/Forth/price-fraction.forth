: as begin parse-word dup while evaluate , repeat 2drop ;

create bounds   as  96 91 86 81 76 71 66 61 56 51 46 41 36 31 26 21 16 11  6  0
create official as 100 98 94 90 86 82 78 74 70 66 62 58 54 50 44 38 32 26 18 10

: official@ ( a-bounds -- +n )
  \ (a+n) - a + b = (a+n) + (b - a) = (b+n)
  [ official bounds - ] literal + @ ;

: round ( n-cents -- n-cents' )
  >r bounds begin dup @ r@ > while cell+ repeat
  r> drop official@ ;
