GoSub, CreateGUI
return

Submit:
Gui, Submit, NoHide
If Input =
 GuiControl,,Output
Else If Input not between 1 and 4
{
 Gui, Destroy
 Sleep, 500
 GoSub, CreateGUI
}
Else {
 GuiControlGet, string,,Text%Input%
 GuiControl,,Output,% SubStr(string,4)
}
return

CreateGUI:
list = fee fie,huff and puff,mirror mirror,tick tock
Loop, Parse, list, `,
 Gui, Add, Text, vText%A_Index%, %A_Index%: %A_LoopField%
Gui, Add, Text, ym, Which is from the three pigs?
Gui, Add, Edit, vInput gSubmit
Gui, Add, Edit, vOutput
Gui, Show
return

GuiClose:
ExitApp
