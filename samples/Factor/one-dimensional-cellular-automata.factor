USING: bit-arrays io kernel locals math sequences ;
IN: cellular

: bool-sum ( bool1 bool2 -- sum )
    [ [ 2 ] [ 1 ] if ]
    [ [ 1 ] [ 0 ] if ] if ;
:: neighbours ( index world -- # )
    index [ 1 - ] [ 1 + ] bi [ world ?nth ] bi@ bool-sum ;
: count-neighbours ( world -- neighbours )
    [ length iota ] keep [ neighbours ] curry map ;

: life-law ( alive? neighbours -- alive? )
    swap [ 1 = ] [ 2 = ] if ;
: step ( world -- world' )
    dup count-neighbours [ life-law ] ?{ } 2map-as ;
: print-cellular ( world -- )
    [ CHAR: # CHAR: _ ? ] "" map-as print ;
: main-cellular ( -- )
    ?{ f t t t f t t f t f t f t f t f f t f f }
    10 [ dup print-cellular step ] times print-cellular ;
MAIN: main-cellular
