: halving ( n -- )
  begin  dup 0 >
  while  cr dup .  2/
  repeat drop ;
1024 halving
