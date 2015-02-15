: print-exists? ( path -- )
    [ write ": " write ] [ exists? "exists." "doesn't exist." ? print ] bi ;

{ "input.txt" "/input.txt" "docs" "/docs" } [ print-exists? ] each
