USING: io kernel math sequences ;
IN: sierpinski

: iterate-triangle ( triange spaces -- triangle' )
    [ [ dup surround ] curry map ]
    [ drop [ dup " " glue ] map ] 2bi append ;

: (sierpinski) ( triangle spaces n -- triangle' )
    dup 0 = [ 2drop "\n" join ] [
        [
            [ iterate-triangle ]
            [ nip dup append ] 2bi
        ] dip 1 - (sierpinski)
    ] if ;

: sierpinski ( n -- )
    [ { "*" } " " ] dip (sierpinski) print ;
