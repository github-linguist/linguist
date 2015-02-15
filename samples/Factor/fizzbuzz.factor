USING: math kernel io math.ranges ;
IN: fizzbuzz
: fizz ( n -- str ) 3 divisor? "Fizz" "" ? ;
: buzz ( n -- str ) 5 divisor? "Buzz" "" ? ;
: fizzbuzz ( n -- str ) dup [ fizz ] [ buzz ] bi append [ number>string ] [ nip ] if-empty ;
: main ( -- ) 100 [1,b] [ fizzbuzz print ] each ;
MAIN: main
