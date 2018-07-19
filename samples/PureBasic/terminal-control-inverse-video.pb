If OpenConsole()
  ConsoleColor(0, 15) ;use the colors black (background) and white (forground)
  PrintN("Inverse Video")
  ConsoleColor(15, 0) ;use the colors white (background) and black (forground)
  PrintN("Normal Video")

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
