for i, seq in ["nacci", "lucas"]
    Loop, 9 {
        Out .= seq "(" A_Index + 1 "): "
        for key, val in NStepSequence(i, 1, A_Index + 1, 15)
            Out .= val (A_Index = 15 ? "`n" : "`, ")
    }
MsgBox, % Out

NStepSequence(v1, v2, n, k) {
    a := [v1, v2]
    Loop, % k - 2 {
        a[j := A_Index + 2] := 0
        Loop, % j < n + 2 ? j - 1 : n
            a[j] += a[j - A_Index]
    }
    return, a
}
