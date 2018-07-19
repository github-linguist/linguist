If OpenConsole()

  Print("Enter a time(milliseconds) to sleep: ")
  x.i = Val(Input())
  PrintN("Sleeping...")
  Delay(x) ;in milliseconds
  PrintN("Awake!")
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
