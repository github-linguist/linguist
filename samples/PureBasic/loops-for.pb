If OpenConsole()
  Define i, j
  For i=1 To 5
    For j=1 To i
      Print("*")
    Next j
    PrintN("")
  Next i
  Print(#LFCR$+"Press ENTER to quit"): Input()
  CloseConsole()
EndIf
