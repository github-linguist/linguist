TUPLE: linked-list data next ;

: <linked-list> ( data -- linked-list )
    linked-list new swap >>data ;
