USING: arrays kernel math multiline sequences ;
IN: ethiopian-multiplication

/*
This function is built-in
: odd? ( n -- ? ) 1 bitand 1 number= ;
*/

: double ( n -- 2*n ) 2 * ;
: halve ( n -- n/2 ) 2 /i ;

: ethiopian-mult ( a b -- a*b )
    [ 0 ] 2dip
    [ dup 0 > ] [
        [ odd? [ + ] [ drop ] if ] 2keep
        [ double ] [ halve ] bi*
    ] while 2drop ;
