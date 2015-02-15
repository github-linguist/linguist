TUPLE: example-pair name value ;

: sort-by-name ( seq -- seq' ) [ [ name>> ] compare ] sort ;
