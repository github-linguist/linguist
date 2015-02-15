: hail-next ( n -- n )
  dup 1 and if 3 * 1+ else 2/ then ;
: .hail ( n -- )
  begin dup . dup 1 > while hail-next repeat drop ;
: hail-len ( n -- n )
  1 begin over 1 > while swap hail-next swap 1+ repeat nip ;

27 hail-len . cr
27 .hail cr

: longest-hail ( max -- )
  0 0 rot 1+ 1 do    ( n length )
    i hail-len 2dup < if
      nip nip i swap
    else drop then
  loop
  swap . ." has hailstone sequence length " . ;

100000 longest-hail
