Start:=A_TickCount
counter=0
SetTimer, timer, 500
return

timer:
Send % ++Counter "`n"
return

^c::
SetTimer, timer, off
SetFormat, float, 0.3
Send, % "Task took " (A_TickCount-Start)/1000 " Seconds"
ExitApp
return
