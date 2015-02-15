Procedure.s middleThreeDigits(x.q)
  Protected x$, digitCount

  If x < 0: x = -x: EndIf

  x$ = Str(x)
  digitCount = Len(x$)
  If digitCount < 3
    ProcedureReturn "invalid input: too few digits"
  ElseIf digitCount % 2 = 0
    ProcedureReturn "invalid input: even number of digits"
  EndIf

  ProcedureReturn Mid(x$,digitCount / 2, 3)
EndProcedure

If OpenConsole()
  Define testValues$ = "123 12345 1234567 987654321 10001 -10001 -123 -100 100 -12345 1 2 -1 -10 2002 -2002 0"

  Define i, value.q, numTests = CountString(testValues$, " ") + 1
  For i = 1 To numTests
    value = Val(StringField(testValues$, i, " "))
    PrintN(RSet(Str(value), 12, " ") + " : " + middleThreeDigits(value))
  Next

  Print(#crlf$ + #crlf$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
