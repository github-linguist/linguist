AutoTrim, Off
draw =
(
 ______  __  __  __  __
/\  __ \/\ \_\ \/\ \/ /
\ \  __ \ \  __ \ \  _"-.
 \ \_\ \_\ \_\ \_\ \_\ \_\
  \/_/\/_/\/_/\/_/\/_/\/_/
)
Gui, +ToolWindow
Gui, Color, 1A1A1A, 1A1A1A
Gui, Font, s8 cLime w800, Courier New
Gui, Add, text, x4 y0 , % " " draw
Gui, Show, w192 h82, AHK in 3D
return

GuiClose:
ExitApp
