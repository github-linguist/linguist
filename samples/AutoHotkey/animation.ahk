SetTimer, Animate ; Timer runs every 250 ms.
String := "Hello World "
Gui, Add, Text, vS gRev, %String%
Gui, +AlwaysOnTop -SysMenu
Gui, Show
Return

Animate:
	String := (!Reverse) ? (SubStr(String, 0) . Substr(String, 1, StrLen(String)-1)) : (SubStr(String, 2) . SubStr(String, 1, 1))
	GuiControl,,S, %String%
return

Rev: ; Runs whenever user clicks on the text control
	Reverse := !Reverse
return
