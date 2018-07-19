: print-comma-list ( n -- )
    [ [1,b] ] keep '[
        [ number>string write ]
        [ _ = [ ", " write ] unless ] bi
    ] each nl ;
