defer (compare)
' - is (compare) \ default to numbers

: cstr-compare ( cstr1 cstr2 -- <=> ) \ counted strings
  swap count rot count compare ;

: mid ( u l -- mid ) tuck - 2/ -cell and + ;

: bsearch ( item upper lower -- where found? )
  rot >r
  begin  2dup >
  while  2dup mid
         dup @ r@ (compare)
         dup
  while  0<
         if   nip cell+   ( upper mid+1 )
         else rot drop swap ( mid lower )
         then
  repeat drop nip nip             true
  else   max ( insertion-point ) false
  then
  r> drop ;

create test 2 , 4 , 6 , 9 , 11 ,   99 ,
: probe ( n -- ) test 5 cells bounds bsearch . @ . cr ;
1 probe \ 0 2
2 probe \ -1 2
3 probe \ 0 4
10 probe \ 0 11
11 probe \ -1 11
12 probe \ 0 99
