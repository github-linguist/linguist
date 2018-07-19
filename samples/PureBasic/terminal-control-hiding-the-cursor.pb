#cursorSize = 10 ;use a full sized cursor

If OpenConsole()
  Print("Press any key to toggle cursor: ")
  EnableGraphicalConsole(1)
  height = #cursorSize
  ConsoleCursor(height)
  Repeat
    If Inkey()
      height ! #cursorSize
      ConsoleCursor(height)
    EndIf
  ForEver
EndIf
