( scratchpad ) : 2ifte ( ..a ?0 ?1 quot0: ( ..a -- ..b ) quot1: ( ..a -- ..b ) quot2: ( ..a -- ..b ) quot3: ( ..a -- ..b ) -- ..b )
[ [ if ] curry curry ] 2bi@ if ; inline
( scratchpad ) 3 [ 0 > ] [ even? ] bi [ 0 ] [ 1 ] [ 2 ] [ 3 ] 2ifte .
2
