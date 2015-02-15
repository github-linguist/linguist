: fhorner ( coeffs len F: x -- F: val )
  0e
  floats bounds ?do
    fover f*  i f@ f+
  1 floats +loop
  fswap fdrop ;

create coeffs 6e f, -4e f, 7e f, -19e f,

coeffs 4 3e fhorner f.    \ 128.
