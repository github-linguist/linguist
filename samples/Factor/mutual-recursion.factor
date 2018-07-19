DEFER: F
: M ( n -- n' ) dup 0 = [ dup 1 - M F - ] unless ;
: F ( n -- n' ) dup 0 = [ drop 1 ] [ dup 1 - F M - ] if ;
