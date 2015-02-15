: f(x)  fdup fsqrt fswap 3e f** 5e f* f+ ;

4e2 fconstant f-too-big

11 Constant #Elements

: float-array ( compile: n -- / run: n -- addr )
    create
        floats allot
    does>
        swap floats + ;

#Elements float-array vec

: get-it  ( -- )
    ." Enter " #Elements . ." numbers:" cr
    #Elements 0 DO
        ." > " pad 25 accept cr
        pad swap >float 0= abort" Invalid Number"
        i vec F!
    LOOP ;

: reverse-it ( -- )
    #Elements 2/  0 DO
        i vec F@  #Elements i - 1- vec F@
        i vec F!  #Elements i - 1- vec F!
    LOOP ;

: do-it ( -- )
    #Elements 0 DO
        i vec F@ fdup f. [char] : emit space
	f(x) fdup f-too-big f> IF
            fdrop ." too large"
        ELSE
            f.
        THEN cr
    LOOP ;

: tpk  ( -- )
    get-it reverse-it do-it ;
