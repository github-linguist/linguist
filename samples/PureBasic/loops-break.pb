If OpenConsole()

  Repeat
    a = Random(19)
    PrintN(Str(a))
    If a = 10
      Break
    EndIf
    b = Random(19)
    PrintN(Str(b))
    PrintN("")
  ForEver

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
