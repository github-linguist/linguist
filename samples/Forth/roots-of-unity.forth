: f0. ( f -- )
  fdup 0e 0.001e f~ if fdrop 0e then f. ;
: .roots ( n -- )
  dup 1 do
    pi i 2* 0 d>f f* dup 0 d>f f/          ( F: radians )
    fsincos cr ." real " f0. ." imag " f0.
  loop drop ;

3 set-precision
5 .roots
