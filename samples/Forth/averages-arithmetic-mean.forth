: fmean ( addr n -- f )
  0e
  dup 0= if 2drop exit then
  tuck floats bounds do
    i f@ f+
  1 floats +loop
  0 d>f f/ ;

create test 3e f, 1e f, 4e f, 1e f, 5e f, 9e f,
test 6 fmean f.     \ 3.83333333333333
