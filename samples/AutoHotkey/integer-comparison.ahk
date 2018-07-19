Gui, Add, Edit
Gui, Add, UpDown, vVar1
Gui, Add, Edit
Gui, Add, UpDown, vVar2
Gui, Add, Button, Default, Submit
Gui, Show
Return

ButtonSubmit:
  Gui, Submit, NoHide
  If (Var1 = Var2)
    MsgBox, % Var1 "=" Var2
  Else If (Var1 < Var2)
    MsgBox, % Var1 "<" Var2
  Else If (Var1 > Var2)
    MsgBox, % Var1 ">" Var2
Return

GuiClose:
  ExitApp
