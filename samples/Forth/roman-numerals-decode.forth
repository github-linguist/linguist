create (arabic)
  1000 128 * char M + ,
   500 128 * char D + ,
   100 128 * char C + ,
    50 128 * char L + ,
    10 128 * char X + ,
     5 128 * char V + ,
     1 128 * char I + ,
does>
  7 cells bounds do
    i @ over over 127 and = if nip 7 rshift leave else drop then
  1 cells +loop dup
;

: >arabic
  0 dup >r >r
  begin
    over over
  while
    c@ dup (arabic) rot <>
  while
    r> over r> over over > if 2* negate + else drop then + swap >r >r 1 /string
  repeat then drop 2drop r> r> drop
;

s" MCMLXXXIV" >arabic .
