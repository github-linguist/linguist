: rms ( faddr len -- frms )
  dup >r 0e
  floats bounds do
    i f@ fdup f* f+
  float +loop
  r> s>f f/ fsqrt ;

create test 1e f, 2e f, 3e f, 4e f, 5e f, 6e f, 7e f, 8e f, 9e f, 10e f,
test 10 rms f.    \ 6.20483682299543
