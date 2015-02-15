USING: assocs combinators kernel math.functions prettyprint sequences ;
IN: rosettacode.first-class-functions

CONSTANT: A { [ sin ] [ cos ] [ 3 ^ ] }
CONSTANT: B { [ asin ] [ acos ] [ 1/3 ^ ] }

: compose-all ( seq1 seq2 -- seq ) [ compose ] 2map ;

: test-fcf ( -- )
    0.5 A B compose-all
    [ call( x -- y ) ] with map . ;
