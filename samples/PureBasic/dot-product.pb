Procedure dotProduct(Array a(1),Array b(1))
  Protected i, sum, length = ArraySize(a())

  If ArraySize(a()) = ArraySize(b())
    For i = 0 To length
      sum + a(i) * b(i)
    Next
  EndIf

  ProcedureReturn sum
EndProcedure

If OpenConsole()
  Dim a(2)
  Dim b(2)

  a(0) = 1 : a(1) = 3 : a(2) = -5
  b(0) = 4 : b(1) = -2 : b(2) = -1

  PrintN(Str(dotProduct(a(),b())))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
