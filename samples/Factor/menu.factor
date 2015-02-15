USE: formatting

: print-menu ( seq -- )
    [ 1 + swap "%d - %s\n" printf ] each-index
    "Your choice? " write flush ;

: select ( seq -- result )
    dup print-menu
    readln string>number [
        1 - swap 2dup bounds-check?
        [ nth ] [ nip select ] if
    ] [ select ] if* ;
