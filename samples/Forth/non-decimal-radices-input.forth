: parse# ( str len -- u true | false )
   0. 2SWAP DUP >R >NUMBER NIP NIP
   R> <> DUP 0= IF NIP THEN ;

: base# ( str len base -- u true | false )
  BASE @ >R  BASE !  parse#  R> BASE ! ;
