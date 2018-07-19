USING: kernel math math.matrices sequences ;

: my-m^n ( m n -- m' )
    dup 0 < [ "no negative exponents" throw ] [
        [ drop length identity-matrix ]
        [ swap '[ _ m. ] times ] 2bi
    ] if ;
