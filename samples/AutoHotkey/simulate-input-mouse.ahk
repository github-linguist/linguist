WinActivate, ahk_class MozillaUIWindowClass
Click 200, 200 right  ; relative to external window (firefox)
sleep, 2000
WinMinimize
CoordMode, Mouse, Screen
Click 400, 400 right  ; relative to top left corner of the screen.
