For i=2008 To 2037
  If DayOfWeek(Date(i,12,25,0,0,0))=0
    PrintN(Str(i))
  EndIf
Next
