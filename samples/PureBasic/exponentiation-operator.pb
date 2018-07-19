Procedure powI(base, exponent)
  Protected i, result.d
  If exponent < 0
    If base = 1
      result = 1
    EndIf
    ProcedureReturn result
  EndIf
  result = 1
  For i = 1 To exponent
    result * base
  Next
  ProcedureReturn result
EndProcedure

Procedure.f powF(base.f, exponent)
  Protected i, magExponent = Abs(exponent), result.d
  If base <> 0
    result = 1.0
    If exponent <> 0
      For i = 1 To magExponent
        result * base
      Next
      If exponent < 0
        result = 1.0 / result
      EndIf
    EndIf
  EndIf
  ProcedureReturn result
EndProcedure

If OpenConsole()
  Define x, a.f, exp

  x = Random(10) - 5
  a = Random(10000) / 10000 * 10
  For exp = -3 To 3
    PrintN(Str(x) + " ^ " + Str(exp) + " = " + Str(powI(x, exp)))
    PrintN(StrF(a) + " ^ " + Str(exp) + " = " + StrF(powF(a, exp)))
    PrintN("--------------")
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
