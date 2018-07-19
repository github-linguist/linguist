If OpenConsole()
  PrintN("Background color#     00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15")
  PrintN("                      -----------------------------------------------")
  Define Foreground, Background
  For Foreground = 0 To 15
    ConsoleColor(7, 0) ;grey foreground, black background
    Print("Foreground color# " + RSet(Str(Foreground), 2, "0") + "  ")
    For Background = 0 To 15
      ConsoleColor(Foreground, Background)
      Print(RSet(Str(Foreground), 2, "0"))
      ConsoleColor(7, 0) ;grey foreground, black background
      Print(" ")
    Next
    PrintN("")
  Next

  ConsoleColor(7, 0) ;grey foreground, black background
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
