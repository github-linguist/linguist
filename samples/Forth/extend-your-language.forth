\ in this construct, either of the ELSE clauses may be omitted, just like IF-THEN.

: BOTH    postpone IF   postpone IF ; immediate
: ORELSE  postpone THEN postpone ELSE postpone IF ; immediate
: NEITHER postpone THEN postpone THEN ; immediate

: fb ( n -- )
  dup 5 mod 0=  over 3 mod 0=
  BOTH   ." FizzBuzz "
  ELSE   ." Fizz "
  ORELSE ." Buzz "
  ELSE   dup .
  NEITHER drop ;
: fizzbuzz ( n -- ) 0 do i 1+ fb loop ;
