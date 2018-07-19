fvariable step

defer method ( fn F: x -- fn[x] )

: left                    execute ;
: right  step f@       f+ execute ;
: mid    step f@ 2e f/ f+ execute ;
: trap
  dup fdup  left
      fswap right f+  2e f/ ;
: simpson
  dup fdup  left
  dup fover mid 4e f* f+
      fswap right f+  6e f/ ;

: set-step ( n F: a b -- n F: a )
  fover f- dup 0 d>f f/ step f! ;

: integrate ( xt n F: a b -- F: sigma )
  set-step
  0e
  0 do
    dup fover method f+
    fswap step f@ f+ fswap
  loop
  drop fnip
  step f@ f* ;
 \ testing similar to the D example
: test
  ' is method  ' 4 -1e 2e integrate f. ;

: fn1  fsincos f+ ;
: fn2  fdup f* 4e f* 1e f+ 2e fswap f/ ;

7 set-precision
test left    fn2  \ 2.456897
test right   fn2  \ 2.245132
test mid     fn2  \ 2.496091
test trap    fn2  \ 2.351014
test simpson fn2  \ 2.447732
