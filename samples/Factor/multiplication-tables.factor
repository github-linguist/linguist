USING: io kernel math math.parser math.ranges sequences ;
IN: multiplication-table

: print-row ( n -- )
    [ number>string 2 CHAR: space pad-head write " |" write ]
    [ 1 - [ "    " write ] times ]
    [
        dup 12 [a,b]
        [ * number>string 4 CHAR: space pad-head write ] with each
    ] tri nl ;

: print-table ( -- )
    "    " write
    1 12 [a,b] [ number>string 4 CHAR: space pad-head write ] each nl
    "   +" write
    12 [ "----" write ] times nl
    1 12 [a,b] [ print-row ] each ;
