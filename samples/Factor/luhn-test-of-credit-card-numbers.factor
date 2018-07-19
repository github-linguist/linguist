USING: kernel math math.parser math.order math.ranges sequences ;
IN: luhn

: reversed-digits ( n -- list )
    { } swap
    [ dup 0 > ]
        [ 10 /mod  swapd suffix  swap ]
    while drop ;

: luhn-digit  ( n -- n )
    reversed-digits dup length iota [
        2dup swap nth
        swap odd? [ 2 *  10 /mod + ] when
    ] map sum 10 mod
    nip ;

: luhn? ( n -- ? )
    luhn-digit 0 = ;
