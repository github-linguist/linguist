: checked-array
  CREATE ( size -- ) DUP , CELLS ALLOT
  DOES> ( i -- a+i )
    2DUP @ 0 SWAP WITHIN IF
      SWAP 1+ CELLS +
    ELSE
      1 THROW
    THEN ;

8 checked-array myarray

: safe-access ( i -- a[i] )
  ['] myarray CATCH 1 = IF ." Out of bounds!" 0 THEN ;
