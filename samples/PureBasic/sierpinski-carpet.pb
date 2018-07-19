Procedure in_carpet(x,y)
  While x>0 And y>0
    If x%3=1 And y%3=1
      ProcedureReturn #False
    EndIf
    y/3: x/3
  Wend
  ProcedureReturn #True
EndProcedure

Procedure carpet(n)
  Define i, j, l=Pow(3,n)-1
  For i=0 To l
    For j=0 To l
      If in_carpet(i,j)
        Print("#")
      Else
        Print(" ")
      EndIf
    Next
    PrintN("")
  Next
EndProcedure
