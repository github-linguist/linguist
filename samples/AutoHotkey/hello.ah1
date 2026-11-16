Gui, Add, Text,, Cool Hotkey
Gui, Add, Hotkey, vMyHotkey, ^l
Gui, Add, Button, gRegister, Register Hotkey
Gui, Show
Return

Register:
try HotKey, %MyHotkey%, Off
GuiControlGet, MyHotkey
HotKey, %MyHotkey%, MyLabel
Return

MyLabel:
MsgBox % A_ThisLabel
Return

GuiClose:
ExitApp
