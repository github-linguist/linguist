USING: binary-search kernel math.order ;

: binary-search ( seq elt -- index/f )
    [ [ <=> ] curry search ] keep = [ drop f ] unless ;
