: strip ( buf len -- buf len' )  \ repacks buffer, so len' <= len
  over + over swap over ( buf dst limit src )
  do
    i c@ 32 127 within if
      i c@ over c! char+
    then
  loop
  over - ;
