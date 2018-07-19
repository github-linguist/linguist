A := ArithmeticMean(1, 10)
G := GeometricMean(1, 10)
H := HarmonicMean(1, 10)

If G Between %H% And %A%
    Result := "True"
Else
    Result := "False"

MsgBox, %A%`n%G%`n%H%`n%Result%


;---------------------------------------------------------------------------
ArithmeticMean(a, b) { ; of integers a through b
;---------------------------------------------------------------------------
    n := b - a + 1
    Loop, %n%
        Sum += (a + A_Index - 1)
    Return, Sum / n
}


;---------------------------------------------------------------------------
GeometricMean(a, b) { ; of integers a through b
;---------------------------------------------------------------------------
    n := b - a + 1
    Prod := 1
    Loop, %n%
        Prod *= (a + A_Index - 1)
    Return, Prod ** (1 / n)
}


;---------------------------------------------------------------------------
HarmonicMean(a, b) { ; of integers a through b
;---------------------------------------------------------------------------
    n := b - a + 1
    Loop, %n%
        Sum += 1 / (a + A_Index - 1)
    Return, n / Sum
}
