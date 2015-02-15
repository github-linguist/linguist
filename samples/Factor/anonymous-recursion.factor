USING: kernel math ;
IN: rosettacode.fibonacci.ar

: fib ( n -- m )
    dup 0 < [ "fib of negative" throw ] when
    [
        ! If n < 2, then drop q, else find q(n - 1) + q(n - 2).
        [ dup 2 < ] dip swap [ drop ] [
            [ [ 1 - ] dip dup call ]
            [ [ 2 - ] dip dup call ] 2bi +
        ] if
    ] dup call( n q -- m ) ;
