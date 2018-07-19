: newton-cooling-law ( f: temp -- f: temp' )
  20e f-  -0.07e f* ;

: euler ( f: y0  xt step end -- )
  1+ 0 do
    cr i . fdup f.
    fdup over execute
    dup s>f f* f+
  dup +loop
  2drop fdrop ;

100e  ' newton-cooling-law  2 100 euler cr
100e  ' newton-cooling-law  5 100 euler cr
100e  ' newton-cooling-law 10 100 euler cr
