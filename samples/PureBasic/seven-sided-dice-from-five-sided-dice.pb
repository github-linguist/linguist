Procedure dice5()
  ProcedureReturn Random(4) + 1
EndProcedure

Procedure dice7()
  Protected x

  x = dice5() * 5 + dice5() - 6
  If x > 20
    ProcedureReturn dice7()
  EndIf

  ProcedureReturn x % 7 + 1
EndProcedure
