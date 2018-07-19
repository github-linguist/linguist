create x  1e f, 2e       f, 3e       f, 1e11       f,
create y  1e f, 2e fsqrt f, 3e fsqrt f, 1e11 fsqrt f,

: main
  s" sqrt.txt" w/o open-file throw  to outfile-id

  4 0 do
    4 set-precision
    x i floats + f@ f.
    6 set-precision
    y i floats + f@ f. cr
  loop

  outfile-id  stdout to outfile-id
  close-file throw ;
