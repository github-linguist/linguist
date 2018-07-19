SetTimer, internal, 1000
Return

internal:  ; fire on a timer
  TrayTip, internal, internal event!`npress F2 for external event
  SetTimer, internal, off
Return

F2::   ; external event: fire on F2 key press
  TrayTip, external, f2 key pressed
Return
