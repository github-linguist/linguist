Loop, 5 {
    Output .= "Degree " (i := A_Index) ": "
    Loop, 10
        Output .= MultiFact(A_Index, i) (A_Index = 10 ? "`n" : ", ")
}
MsgBox, % Output

MultiFact(n, d) {
    Result := n
    while 1 < n -= d
        Result *= n
    return, Result
}
