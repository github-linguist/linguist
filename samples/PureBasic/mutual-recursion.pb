Declare M(n)

Procedure F(n)
  If n = 0
    ProcedureReturn 1
  ElseIf n > 0
    ProcedureReturn n - M(F(n - 1))
  EndIf
EndProcedure

Procedure M(n)
  If n = 0
    ProcedureReturn 0
  ElseIf n > 0
    ProcedureReturn n - F(M(n - 1))
  EndIf
EndProcedure

Define i
If OpenConsole()

  For i = 0 To 19
    Print(Str(F(i)))
    If i = 19
      Continue
    EndIf
    Print(", ")
  Next

  PrintN("")
  For i = 0 To 19
    Print(Str(M(i)))
    If i = 19
      Continue
    EndIf
    Print(", ")
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
