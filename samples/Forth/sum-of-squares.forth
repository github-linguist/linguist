: fsum**2 ( addr n -- f )
  0e
  dup 0= if 2drop exit then
  floats bounds do
    i f@ fdup f* f+
  1 floats +loop ;

create test 3e f, 1e f, 4e f, 1e f, 5e f, 9e f,
test 6 fsum**2 f.     \ 133.
