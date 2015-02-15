USING: io kernel math.matrices math.parser prettyprint
sequences ;
IN: rosettacode.runtime2darray

: set-Mi,j ( elt {i,j} matrix -- )
[ first2 swap ] dip nth set-nth ;
: Mi,j ( {i,j} matrix -- elt )
[ first2 swap ] dip nth nth ;

: example ( -- )
readln readln [ string>number ] bi@ zero-matrix ! create the array
[ [ 42 { 0 0 } ] dip set-Mi,j ] ! set the { 0 0 } element to 42
[ [ { 0 0 } ] dip Mi,j . ] ! read the { 0 0 } element
bi ;
