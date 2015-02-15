: flog2 ( f -- f ) fln 2e fln f/ ;

create freq 256 cells allot

: entropy ( str len -- f )
  freq 256 cells erase
  tuck
  bounds do
    i c@ cells freq +
    1 swap +!
  loop
  0e
  256 0 do
    i cells freq + @ ?dup if
      s>f dup s>f f/
      fdup flog2 f* f-
    then
  loop
  drop ;

s" 1223334444" entropy f.     \ 1.84643934467102  ok
