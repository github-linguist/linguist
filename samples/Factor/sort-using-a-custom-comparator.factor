: my-compare ( s1 s2 -- <=> )
    2dup [ length ] compare invert-comparison
    dup +eq+ = [ drop [ >lower ] compare ] [ 2nip ] if ;

{ "this" "is" "a" "set" "of" "strings" "to" "sort" } [ my-compare ] sort
