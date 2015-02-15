: fib<= ( n -- n )
    >r 0 1 BEGIN dup r@ <= WHILE  tuck +  REPEAT  drop rdrop ;

: z. ( n -- )
   dup fib<= dup . -
   BEGIN ?dup WHILE
      dup fib<= dup [char] + emit space . -
   REPEAT ;

: tab  9 emit ;

: zeckendorf ( -- )
    21 0 DO
        cr i 2 .r tab i z.
    LOOP ;
