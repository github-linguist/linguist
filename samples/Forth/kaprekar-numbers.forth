: square ( n - n^2)   dup * ;

\ Return nonzero if n is a Kaprekar number for tens, where tens is a
\ nonzero power of base.
: is-kaprekar? ( tens n n^2 - t)   rot /mod  over >r  + =  r> and ;

\ If n is a Kaprekar number, return is the power of base for which it
\ is Kaprekar.  If n is not a Kaprekar number, return zero.
: kaprekar ( +n - +n1)
    dup square >r
    base @ swap
    begin ( tens n) ( R: n^2)
        over r@ < while
            2dup r@ is-kaprekar? if
                drop  r> drop  exit  then
            swap  base @ *  swap
    repeat
    r> drop  1 = and ;
