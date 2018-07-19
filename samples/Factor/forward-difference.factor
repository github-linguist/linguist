USING: kernel math math.vectors sequences ;
IN: rosetacode

: 1-order ( seq -- seq' )
    [ rest-slice ] keep v- ;

: n-order ( seq n -- seq' )
    dup 0 <=
    [ drop ] [ [ 1-order ] times ] if ;
