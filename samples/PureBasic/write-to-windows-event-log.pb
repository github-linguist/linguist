Procedure WriteToLog(Event_App$,EventMessage$,EvenetType,Computer$)

  Protected wNumStrings.w, lpString=@EventMessage$, lReturnX, CMessageTyp, lparray
  Protected lprawdata=@EventMessage$, rawdata=Len(EventMessage$), Result
  Protected lLogAPIRetVal.l = RegisterEventSource_(Computer$, Event_App$)

  If lLogAPIRetVal
    lReturnX = ReportEvent_(lLogAPIRetVal,EvenetType,0,CMessageTyp,0,wNumStrings,rawdata,lparray,lprawdata
    DeregisterEventSource_(lLogAPIRetVal)
    Result=#True
  EndIf

  ProcedureReturn Result
EndProcedure
