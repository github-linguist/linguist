FUNCTION: char* strdup ( c-string s ) ;

: my-strdup ( str -- str' )
    strdup [ utf8 alien>string ] [ (free) ] bi ;
