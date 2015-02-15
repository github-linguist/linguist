Procedure.q Ackermann(m, n)
  If m = 0
    ProcedureReturn n + 1
  ElseIf  n = 0
    ProcedureReturn Ackermann(m - 1, 1)
  Else
    ProcedureReturn Ackermann(m - 1, Ackermann(m, n - 1))
  EndIf
EndProcedure

Debug Ackermann(3,4)
