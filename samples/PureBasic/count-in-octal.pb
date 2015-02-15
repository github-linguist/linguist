Procedure.s octal(n.q)
  Static Dim digits(20)
  Protected i, j, result.s
  For i = 0 To 20
    digits(i) = n % 8
    n / 8
    If n < 1
      For j = i To 0 Step -1
        result + Str(digits(j))
      Next
      Break
    EndIf
  Next

  ProcedureReturn result
EndProcedure

Define n.q
If OpenConsole()
  While n >= 0
    PrintN(octal(n))
    n + 1
  Wend

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
