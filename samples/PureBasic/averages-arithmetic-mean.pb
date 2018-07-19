Procedure.d mean(List number())
  Protected sum=0

  ForEach number()
    sum + number()
  Next
  ProcedureReturn sum / ListSize(number())
  ; Depends on programm if zero check needed, returns nan on division by zero
EndProcedure
