USING: globs io io.directories kernel regexp sequences ;
IN: walk-directory-non-recursively

: print-files ( path pattern -- )
    [ directory-files ] [ <glob> ] bi* [ matches? ] curry filter
    [ print ] each ;
