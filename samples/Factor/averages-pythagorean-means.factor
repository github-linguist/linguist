: a-mean ( seq -- mean )
    [ sum ] [ length ] bi / ;

: g-mean ( seq -- mean )
    [ product ] [ length recip ] bi ^ ;

: h-mean ( seq -- mean )
    [ length ] [ [ recip ] map-sum ] bi / ;
