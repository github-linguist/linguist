: (look-and-say) ( str -- )
    unclip-slice swap [ 1 ] 2dip [
        2dup = [ drop [ 1 + ] dip ] [
            [ [ number>string % ] dip , 1 ] dip
        ] if
    ] each [ number>string % ] [ , ] bi* ;

: look-and-say ( str -- str' ) [ (look-and-say) ] "" make ;

"1" 10 [ dup print look-and-say ] times print
