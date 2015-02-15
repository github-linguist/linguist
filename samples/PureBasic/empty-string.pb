Procedure.s isStringEmpty(a.s)
  If a
    ProcedureReturn "String is not empty, it contains '" + a + "'."
  Else
    ProcedureReturn "String is empty, or null."
  EndIf
EndProcedure

If OpenConsole()
  Define a.s = ""
  Define b.s = "stuff"
  PrintN(isStringEmpty(a))
  PrintN(isStringEmpty(b))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
