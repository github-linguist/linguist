Coefficients = -19, 7, -4, 6
x := 3

MsgBox, % EvalPolynom(Coefficients, x)



;---------------------------------------------------------------------------
EvalPolynom(Coefficients, x) { ; using Horner's rule
;---------------------------------------------------------------------------
    StringSplit, Co, coefficients, `,, %A_Space%
    Result := 0
    Loop, % Co0
        i := Co0 - A_Index + 1, Result := Result * x + Co%i%
    Return, Result
}
