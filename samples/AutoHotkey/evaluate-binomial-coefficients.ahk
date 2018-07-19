MsgBox, % Round(BinomialCoefficient(5, 3))

;---------------------------------------------------------------------------
BinomialCoefficient(n, k) {
;---------------------------------------------------------------------------
    r := 1
    Loop, % k < n - k ? k : n - k {
        r *= n - A_Index + 1
        r /= A_Index
    }
    Return, r
}
