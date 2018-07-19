USING: formatting kernel math.functions math.parser sequences ;
IN: rosettacode.bignums

: test-bignums ( -- )
    5 4 3 2 ^ ^ ^ number>string
    [ 20 head ] [ 20 tail* ] [ length ] tri
    "5^4^3^2 is %s...%s and has %d digits\n" printf ;
