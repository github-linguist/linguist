USE: math.floats.env

: try-div ( a b -- )
    '[ { +fp-zero-divide+ } [ _ _ /f . ] with-fp-traps ] try ;
