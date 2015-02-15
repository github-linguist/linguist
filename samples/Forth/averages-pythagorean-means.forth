: famean ( faddr n -- f )
  0e
  tuck floats bounds do
    i f@ f+
  float +loop
  0 d>f f/ ;

: fgmean ( faddr n -- f )
  1e
  tuck floats bounds do
    i f@ f*
  float +loop
  0 d>f 1/f f** ;

: fhmean ( faddr n -- f )
  dup 0 d>f  0e
  floats bounds do
    i f@ 1/f f+
  float +loop
  f/ ;

create test 1e f, 2e f, 3e f, 4e f, 5e f, 6e f, 7e f, 8e f, 9e f, 10e f,
test 10 famean fdup f.
test 10 fgmean fdup fdup f.
test 10 fhmean fdup f.
( A G G H )
f>= . f>= .  \ -1 -1
