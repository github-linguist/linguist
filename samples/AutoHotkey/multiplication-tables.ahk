Gui, -MinimizeBox
Gui, Margin, 0, 0
Gui, Font, s9, Fixedsys
Gui, Add, Edit, h0 w0
Gui, Add, Edit, w432 r14 -VScroll
Gosub, Table
Gui, Show,, Multiplication Table
Return

GuiClose:
GuiEscape:
    ExitApp
Return

Table:
    ; top row
    Table := "  x |"
    Loop, 12
        Table .= SubStr("   " A_Index, -3)
    Table .= "`n"

    ; underlines
    Table .= "----+"
    Loop, 48
        Table .= "-"
    Table .= "`n"

    ; table
    Loop, 12 { ; rows
        Table .= SubStr("  " Row := A_Index, -2) " |"
        Loop, 12 ; columns
            Table .= SubStr("    " (A_Index >= Row ? A_Index * Row : ""), -3)
        Table .= "`n"
    }
    GuiControl,, Edit2, %Table%
Return
