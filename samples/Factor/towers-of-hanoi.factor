USING: formatting kernel locals math ;
IN: rosettacode.hanoi

: move ( from to -- )
    "%d->%d\n" printf ;
:: hanoi ( n from to other -- )
    n 0 > [
        n 1 - from other to hanoi
        from to move
        n 1 - other to from hanoi
    ] when ;
