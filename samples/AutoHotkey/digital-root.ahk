p := {}
for key, val in [30,1597,381947,92524902,448944221089]
{
    n := val
    while n > 9
    {
        m := 0
        Loop, Parse, n
            m += A_LoopField
        n := m, i := A_Index
    }
    p[A_Index] := [val, n, i]
}

for key, val in p
    Output .= val[1] ": Digital Root = " val[2] ", Additive Persistence = " val[3] "`n"

MsgBox, 524288, , % Output
