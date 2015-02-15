: decomp ( n -- )
  2
  begin  2dup dup * >=
  while  2dup /mod swap
         if   drop  1+ 1 or    \ next odd number
         else -rot nip  dup .
         then
  repeat
  drop . ;
