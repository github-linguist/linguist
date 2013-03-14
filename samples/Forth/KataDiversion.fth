\ KataDiversion in Forth

\ -- utils

\ empty the stack
: EMPTY
    DEPTH 0 <> IF BEGIN
                    DROP DEPTH 0 =
                  UNTIL
               THEN ;

\ power
: ** ( n1 n2 -- n1_pow_n2 ) 1 SWAP ?DUP IF 0 DO OVER * LOOP THEN NIP ;

\ compute the highest power of 2 below N.
\ e.g. : 31 -> 16, 4 -> 4
: MAXPOW2 ( n -- log2_n ) DUP 1 < IF 1 ABORT" Maxpow2 need a positive value."
                               ELSE DUP 1 = IF 1
                                            ELSE
                                                1 >R
                                                BEGIN ( n |R: i=1)
                                                    DUP DUP I - 2 *
                                                    ( n n 2*[n-i])
                                                    R> 2 * >R ( … |R: i*2)
                                                    > ( n n>2*[n-i] )
                                                UNTIL
                                                R> 2 /
                                            THEN
                               THEN NIP ;

\ -- kata

\ test if the given N has two adjacent 1 bits
\ e.g. : 11 -> 1011 -> -1
\         9 -> 1001 ->  0
: ?NOT-TWO-ADJACENT-1-BITS ( n -- bool )
    \ the word uses the following algorithm :
    \ (stack|return stack)
    \ ( A N | X )  A: 0, X: N LOG2
    \ loop: if N-X > 0 then A++ else A=0 ; X /= 2
    \       return 0 if A=2
    \       if X=1 end loop and return -1
    0 SWAP DUP DUP 0 <> IF
                            MAXPOW2 >R
                            BEGIN
                                DUP I - 0 >= IF 
                                                SWAP DUP 1 = IF 1+ SWAP
                                                             ELSE DROP 1 SWAP I -
                                                            THEN
                                             ELSE NIP 0 SWAP
                                             THEN
                                OVER
                                2 =
                                I 1 = OR
                                R> 2 / >R
                            UNTIL
                            R> 2DROP
                            2 <>
                        ELSE 2DROP INVERT
                        THEN ;

\ return the maximum number which can be made with N (given number) bits
: MAX-NB ( n -- m ) DUP 1 < IF DROP 0 ( 0 )
                            ELSE 
                                DUP IF DUP 2 SWAP ** NIP 1 - ( 2**n - 1 )
                                    THEN
                            THEN ;


\ return the number of numbers which can be made with N (given number) bits
\ or less, and which have not two adjacent 1 bits.
\ see http://www.codekata.com/2007/01/code_kata_fifte.html
: HOW-MANY-NB-NOT-TWO-ADJACENT-1-BITS ( n -- m )
       DUP 1 < IF DUP 0
               ELSE
                   0 SWAP
                   MAX-NB 1 + 0 DO I ?NOT-TWO-ADJACENT-1-BITS - LOOP
               THEN ;

