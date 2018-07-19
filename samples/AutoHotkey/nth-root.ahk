p := 0.000001

MsgBox, % nthRoot( 10, 7131.5**10, p) "`n"
        . nthRoot(  5, 34.0      , p) "`n"
        . nthRoot(  2, 2         , p) "`n"
        . nthRoot(0.5, 7         , p) "`n"


;---------------------------------------------------------------------------
nthRoot(n, A, p) { ; http://en.wikipedia.org/wiki/Nth_root_algorithm
;---------------------------------------------------------------------------
    x1 := A
    x2 := A / n
    While Abs(x1 - x2) > p {
        x1 := x2
        x2 := ((n-1)*x2+A/x2**(n-1))/n
    }
    Return, x2
}
