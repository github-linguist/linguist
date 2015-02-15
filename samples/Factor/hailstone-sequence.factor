! rosetta/hailstone/hailstone.factor
USING: arrays io kernel math math.ranges prettyprint sequences vectors ;
IN: rosetta.hailstone

: hailstone ( n -- seq )
    [ 1vector ] keep
    [ dup 1 number= ]
    [
        dup even? [ 2 / ] [ 3 * 1 + ] if
        2dup swap push
    ] until
    drop ;

<PRIVATE
: main ( -- )
    27 hailstone dup dup
    "The hailstone sequence from 27:" print
    "  has length " write length .
    "  starts with " write 4 head [ unparse ] map ", " join print
    "  ends with " write 4 tail* [ unparse ] map ", " join print

    ! Maps n => { length n }, and reduces to longest Hailstone sequence.
    1 100000 [a,b)
    [ [ hailstone length ] keep 2array ]
    [ [ [ first ] bi@ > ] most ] map-reduce
    first2
    "The hailstone sequence from " write pprint
    " has length " write pprint "." print ;
PRIVATE>

MAIN: main
