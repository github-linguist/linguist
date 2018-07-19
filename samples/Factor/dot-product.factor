USING: kernel math.vectors sequences ;

: dot-product ( u v -- w )
    2dup [ length ] bi@ =
    [ v. ] [ "Vector lengths must be equal" throw ] if ;
