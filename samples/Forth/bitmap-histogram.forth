: histogram ( array gmp -- )
  over 256 cells erase
  dup bdim * over bdata +  swap bdata
  do 1 over i c@ cells + +! loop drop ;
