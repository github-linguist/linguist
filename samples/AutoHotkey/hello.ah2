MyGui := Gui()
MyGui.OnEvent("Close", GuiClose)
MyGui.Add("Text",, "Cool Hotkey")
MyHotkeyCtrl := MyGui.Add("Hotkey", "vMyHotkey", "^l")
MyGui.Add("Button",, "Register Hotkey").OnEvent("Click", Register)
MyGui.Show()
Return

Register(*)
{
global
try Hotkey(MyHotkey, "Off")
MyHotkey := MyHotkeyCtrl.Value
Hotkey(MyHotkey, MyCallback)
Return
}

MyCallback(ThisHotkey*)
{
MsgBox("You pressed: " ThisHotkey[1])
Return
}

GuiClose(*)
{
ExitApp()
}
