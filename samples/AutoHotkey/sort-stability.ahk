Table =
(
UK, London
US, New York
US, Birmingham
UK, Birmingham
)

Gui, Margin, 6
Gui, -MinimizeBox
Gui, Add, ListView, r5 w260 Grid, Orig.Position|Country|City
Loop, Parse, Table, `n, `r
{
    StringSplit, out, A_LoopField, `,, %A_Space%
    LV_Add("", A_Index, out1, out2)
}
LV_ModifyCol(1, "77 Center")
LV_ModifyCol(2, "100 Center")
LV_ModifyCol(3, 79)
Gui, Add, Button, w80, Restore Order
Gui, Add, Button, x+10 wp, Sort Countries
Gui, Add, Button, x+10 wp, Sort Cities
Gui, Show,, Sort stability
Return

GuiClose:
GuiEscape:
ExitApp

ButtonRestoreOrder:
    LV_ModifyCol(1, "Sort")
Return

ButtonSortCountries:
    LV_ModifyCol(2, "Sort")
Return

ButtonSortCities:
    LV_ModifyCol(3, "Sort")
Return
