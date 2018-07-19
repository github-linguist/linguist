If OpenConsole()

  CompilerIf Defined(var, #PB_Variable)
    PrintN("var is defined at first check")
  CompilerElse
    PrintN("var is undefined at first check")
    Define var
  CompilerEndIf

  CompilerIf Defined(var, #PB_Variable)
    PrintN("var is defined at second check")
  CompilerElse
    PrintN("var is undefined at second check")
    Define var
  CompilerEndIf

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()

EndIf
