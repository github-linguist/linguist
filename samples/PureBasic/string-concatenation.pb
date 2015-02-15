If OpenConsole()

  s$ = "hello"
  PrintN( s$ + " literal")
  s2$ = s$ + " literal"
  PrintN(s2$)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
