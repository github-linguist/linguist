F1::  ;; when user hits the F1 key, do the following
WinGetTitle, window, A  ; get identity of active window into a variable
WinMove, %window%, , 100, 100, 800, 800 ; move window to coordinates, 100, 100
                                        ; and change size to 800 x 800 pixels
sleep, 2000
WinHide, % window    ; hide window
TrayTip, hidden, window is hidden, 2
sleep, 2000
WinShow, % window  ; show window again
loop,
{
  inputbox, name, what was the name of your window?
  if (name = window) ; compare window variables for equality
  {
    msgbox you got it
    break
  }
; else try again
}
WinClose, % window
return
