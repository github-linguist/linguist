If OpenConsole()

  x.i = 1024
  While x > 0
    PrintN(Str(x))
    x / 2
  Wend

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
