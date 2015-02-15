setA = John, Bob, Mary, Serena
setB = Jim, Mary, John, Bob
MsgBox,, Singles, % SymmetricDifference(setA, setB)

setA = John, Serena, Bob, Mary, Serena
setB = Jim, Mary, John, Jim, Bob
MsgBox,, Duplicates, % SymmetricDifference(setA, setB)

;---------------------------------------------------------------------------
SymmetricDifference(A, B) { ; returns the symmetric difference of A and B
;---------------------------------------------------------------------------
    StringSplit, A_, A, `,, %A_Space%
    Loop, %A_0%
        If Not InStr(B, A_%A_Index%)
        And Not InStr(Result, A_%A_Index%)
            Result .= A_%A_Index% ", "
    StringSplit, B_, B, `,, %A_Space%
    Loop, %B_0%
        If Not InStr(A, B_%A_Index%)
        And Not InStr(Result, B_%A_Index%)
            Result .= B_%A_Index% ", "
    Return, SubStr(Result, 1, -2)
}
