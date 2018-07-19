If OpenConsole()
  PrintN(Right("knight", Len("knight") - 1))  ;strip the first letter
  PrintN(Left("socks", Len("socks")- 1))      ;strip the last letter
  PrintN(Mid("brooms", 2, Len("brooms") - 2)) ;strip both the first and last letter

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
