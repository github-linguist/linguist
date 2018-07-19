;- Create a linked list to store created windows.
NewList Windows()
Define i, j, dh, dw, flags, err$, x, y

;- Used sub-procedure to simplify the error handling
Procedure HandleError(Result, Text.s,ErrorLine=0,ExitCode=0)
  If Not Result
    MessageRequester("Error",Text)
    End ExitCode
  EndIf
  ProcedureReturn Result
EndProcedure

;- Window handling procedures
Procedure Minimize(window)
  SetWindowState(window,#PB_Window_Minimize)
EndProcedure

Procedure Normalize(window)
  SetWindowState(window,#PB_Window_Normal)
EndProcedure

;- Get enviroment data
HandleError(ExamineDesktops(),  "Failed to examine you Desktop.")
dh=HandleError(DesktopHeight(0),"Could not retrieve DesktopHight")/3
dw=HandleError(DesktopWidth(0), "Could not retrieve DesktopWidth")/3

;- Now, creating 9 windows
flags=#PB_Window_SystemMenu
err$="Failed to open Window"
For i=0 To 8
  j=HandleError(OpenWindow(#PB_Any,i*10,i*10+30,10,10,Str(i),flags),err$)
  SmartWindowRefresh(j, 1)
  AddElement(Windows())
  Windows()=j
Next i
Delay(1000)

;- Call a sub-routine for each Window stored in the list.
ForEach Windows()
  Minimize(Windows())
Next
Delay(1000)
;- and again
ForEach Windows()
  Normalize(Windows())
Next
Delay(1000)

;- Spread them evenly
ForEach Windows()
  ResizeWindow(Windows(),x*dw,y*dh,dw-15,dh-35)
  x+1
  If x>2
    x=0: y+1
  EndIf
Next
Delay(2000)

End
