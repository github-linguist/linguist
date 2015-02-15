Progress, b2 w150 zh0 fs9, CreateLists ...
CreateLists(2 ** (Max:=20))

Progress,, Find Maxima ...
Loop, % Max - 1
    msg .= "Maximum between 2^" A_Index " and 2^" A_Index + 1
        .  " is " GetMax(2 ** A_Index, 2 ** (A_Index + 1), n)
        .  " for n = " n "`n"

Progress,, Find Mallows Number ...
Loop, % 2 ** Max
    If (n_%A_Index% > 0.55)
        MallowsNumber := A_Index
msg .= "Mallows Number = " MallowsNumber

Progress, Off
MsgBox, %msg%

;---------------------------------------------------------------------------
GetMax(a, b, ByRef Item) { ; return max value of a(n)/n between a and b
;---------------------------------------------------------------------------
    Loop {
        IfGreater, a, %b%, Break
        If (Maximum < n_%a%)
            Maximum := n_%a%, Item := a
        a++
    }
    Return, Maximum
}

;---------------------------------------------------------------------------
CreateLists(Lenght) { ; Hofstadter-Conway sequences (using lookups)
;---------------------------------------------------------------------------
    ; create the sequence  a_%A_Index%  [ a(n)   ]
    ;   and  the sequence  n_%A_Index%  [ a(n)/n ]
    ;-----------------------------------------------------------------------
    global
    a_1 := a_2 := n_1 := 1, n_2 := 1 / 2
    Loop, %Lenght% {
        IfLess, A_Index, 3, Continue
        n1 := A_Index - 1
        an1 := a_%n1%
        nan1 := A_Index - an1
        a_%A_Index% := a_%an1% + a_%nan1%
        n_%A_Index% := a_%A_Index% / A_Index
    }
}
