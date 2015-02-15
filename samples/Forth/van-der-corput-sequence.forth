: fvdc ( base n -- f )
  0e 1e ( F: vdc denominator )
  begin dup while
    over s>d d>f f*
    over /mod  ( base rem n )
    swap s>d d>f fover f/
    frot f+ fswap
  repeat 2drop fdrop ;

: test  10 0 do 2 i fvdc cr f. loop ;
