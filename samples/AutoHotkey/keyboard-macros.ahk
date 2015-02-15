Loop, 200  ; loop 200 times while not paused
{
  TrayTip, counting, %A_Index% press alt-p to pause
  Sleep, 1000
}

!p::  ; links alt-p key combination to the method pauseme() (system wide)
  pauseMe()
Return

!r::  ; links alt-r key combination to the method resume()  (system wide)
  resume()
Return

pauseMe()
{
  MsgBox, pausing`, press alt-r to resume
  Pause
}

resume()
{
  TrayTip, resume, resuming, 2
  Pause, off
}
