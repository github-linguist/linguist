USING: combinators kernel math math.functions math.ranges sequences ;

: prime? ( n -- ? )
    {
        { [ dup 2 < ] [ drop f ] }
        { [ dup even? ] [ 2 = ] }
        [ 3 over sqrt 2 <range> [ mod 0 > ] with all? ]
    } cond ;
