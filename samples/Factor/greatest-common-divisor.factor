: gcd ( a b -- c )
    [ abs ] [
        [ nip ] [ mod ] 2bi gcd
    ] if-zero ;
