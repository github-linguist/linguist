create buf1 256 allot
create buf2 256 allot
buf1 value src
buf2 value dest

s" 1" src place

: append-run ( digit run -- )
  dest count +
  tuck c!  1+ c!
  dest c@ 2 + dest c! ;

: next-look-and-say
  0 dest c!
  src 1+ c@  [char] 0  ( digit run )
  src count bounds do
    over i c@ =
    if   1+
    else append-run  i c@ [char] 1
    then
  loop
  append-run
  src dest to src to dest ;

: look-and-say ( n -- )
  0 do next-look-and-say  cr src count type loop ;

10 look-and-say
