SetFormat, FloatFast, 0.5
for i, v in [2, 3, 4, 5, 6] {
    seq .= "Base " v ": "
    Loop, 10
        seq .= VanDerCorput(A_Index - 1, v) (A_Index = 10 ? "`n" : ", ")
}
MsgBox, % seq

VanDerCorput(n, b, r=0) {
    while n
        r += Mod(n, b) * b ** -A_Index, n := n // b
    return, r
}
