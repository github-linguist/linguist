Enumeration
  #TextGadget
  #AddButton
  #SubButton
EndEnumeration

Procedure UpdateGadgets(Value,UpdateValue=0)
  Overmax=0: UnderMin=0
  If Value>=10
    Overmax=1
  ElseIf Value<=0
    UnderMin=1
  EndIf
  DisableGadget(#AddButton,Overmax)
  DisableGadget(#SubButton,UnderMin)
  If UpdateValue
    SetGadgetText(#TextGadget,Str(Value))
  EndIf
EndProcedure

If OpenWindow(0,#PB_Ignore,#PB_Ignore,110,70,"PB-GUI",#PB_Window_SystemMenu)
  StringGadget(#TextGadget,10,10,90,20,"")
  ButtonGadget(#AddButton,10,40,30,20,"+")
  ButtonGadget(#SubButton,70,40,30,20,"-")
  UpdateGadgets(Value,1)
  Repeat
    Event=WaitWindowEvent()
    If Event=#PB_Event_Gadget
      Gadget=EventGadget()
      Select Gadget
        Case #AddButton
          Value+1
          UpdateGadgets(Value,1)
        Case #SubButton
          Value-1
          UpdateGadgets(Value,1)
        Default
          EType=EventType()
          If EType=#PB_EventType_Change
            Value=Val(GetGadgetText(#TextGadget))
            UpdateGadgets(Value)
          EndIf
      EndSelect
    EndIf
    Until Event=#PB_Event_CloseWindow
EndIf
