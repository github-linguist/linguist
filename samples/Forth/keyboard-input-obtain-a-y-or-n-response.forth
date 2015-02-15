: flush ( -- )  \ discard pending input
  begin key? while key drop repeat ;

: y-or-n ( c-addr u -- f )
  flush begin
    cr 2dup type key bl or                  \ note 1.
    dup [char] y = swap [char] n = over or  \ note 2.
    if nip nip exit then
  drop again ;

\ Note 1. KEY BL OR returns a lowercase letter in the case that an
\ uppercase letter was entered, an unchanged lowercase letter in the
\ case that a lowercase letter was entered, and garbage otherwise.  BL
\ returns the ASCII code for a space, 32, which is incidentally the
\ "bit of difference" between ASCII uppercase and lowercase letters.

\ Note 2. this line has the stack effect ( x -- f1 f2 ), where F1 is
\ true only if x='y', and F2 is true only if x='y' OR if x='n'.

\ I think these expressions aren't too clever, but they _are_ rather
\ optimized for the task at hand.  This might be more conventional:

: y-or-n ( c-addr u -- f )
  flush begin
    cr 2dup type key case
      [char] y of 2drop true  exit endof
      [char] Y of 2drop true  exit endof
      [char] n of 2drop false exit endof
      [char] N of 2drop false exit endof
  endcase again ;
