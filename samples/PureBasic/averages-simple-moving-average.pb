Procedure.d SMA(Number, Period=0)
  Static P
  Static NewList L()
  Protected Sum=0
  If Period<>0
    P=Period
  EndIf
  LastElement(L())
  AddElement(L())
  L()=Number
  While ListSize(L())>P
    FirstElement(L())
    DeleteElement(L(),1)
  Wend
  ForEach L()
    sum+L()
  Next
  ProcedureReturn sum/ListSize(L())
EndProcedure
