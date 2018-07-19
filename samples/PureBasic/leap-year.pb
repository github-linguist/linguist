Procedure isLeapYear(Year)
  If (Year%4=0 And Year%100) Or Year%400=0
    ProcedureReturn #True
  Else
    ProcedureReturn #False
  EndIf
EndProcedure
