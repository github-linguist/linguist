USING: accessors io kernel math math.functions math.parser
sequences ;
IN: standard-deviator

TUPLE: standard-deviator sum sum^2 n ;

: <standard-deviator> ( -- standard-deviator )
    0.0 0.0 0 standard-deviator boa ;

: current-std ( standard-deviator -- std )
    [ [ sum^2>> ] [ n>> ] bi / ]
    [ [ sum>> ] [ n>> ] bi / sq ] bi - sqrt ;

: add-value ( value standard-deviator -- )
    [ nip [ 1 + ] change-n drop ]
    [ [ + ] change-sum drop ]
    [ [ [ sq ] dip + ] change-sum^2 drop ] 2tri ;

: main ( -- )
    { 2 4 4 4 5 5 7 9 }
    <standard-deviator> [ [ add-value ] curry each ] keep
    current-std number>string print ;
