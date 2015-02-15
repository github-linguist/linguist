If OpenConsole()
  PrintN(FormatDate("Y = %yyyy  M = %mm  D = %dd, %hh:%ii:%ss", 0))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
