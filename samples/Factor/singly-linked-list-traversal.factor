: list-each ( linked-list quot: ( data -- ) -- )
    [ [ data>> ] dip call ]
    [ [ next>> ] dip over [ list-each ] [ 2drop ] if ] 2bi ; inline recursive

SYMBOLS: A B C ;

A <linked-list>
[ C <linked-list> list-insert ] keep
[ B <linked-list> list-insert ] keep

[ . ] list-each
