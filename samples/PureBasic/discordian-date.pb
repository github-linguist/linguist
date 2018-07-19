Procedure.s Discordian_Date(Y, M, D)
  Protected DoY=DayOfYear(Date(Y,M,D,0,0,0)), Yold$=Str(Y+1166)
  Dim S.s(4)
  S(0)="Chaos": S(1)="Discord": S(2)="Confusion": S(3)="Bureaucracy"
  S(4)="The Aftermath"
  If (Y%4=0 And Y%100) Or Y%400=0
    If M=2 And D=29
      ProcedureReturn "St. Tib's Day, YOLD " + Yold$
    ElseIf DoY>=2*30
      DoY-1
    EndIf
  EndIf
  ProcedureReturn S(DoY/73)+" "+Str(DoY%73)+", Yold "+Yold$
EndProcedure
