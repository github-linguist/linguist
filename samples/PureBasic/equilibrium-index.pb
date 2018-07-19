If OpenConsole()
  Define i, c=CountProgramParameters()-1
  For i=0 To c
    Define j, LSum=0, RSum=0
    For j=0 To c
      If j<i
        LSum+Val(ProgramParameter(j))
      ElseIf j>i
        RSum+Val(ProgramParameter(j))
      EndIf
    Next j
    If LSum=RSum: PrintN(Str(i)): EndIf
  Next i
EndIf
