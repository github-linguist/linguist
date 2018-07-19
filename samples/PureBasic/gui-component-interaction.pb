Enumeration
  #StringGadget
  #Increment
  #Random
EndEnumeration

If OpenWindow(0,#PB_Ignore,#PB_Ignore,180,50,"PB-GUI",#PB_Window_SystemMenu)
  StringGadget(#StringGadget,5,5,170,20,"",#PB_String_Numeric)
  ButtonGadget(#Increment,5,25,80,20, "Increment")
  ButtonGadget(#Random,  90,25,80,20, "Random")
  Repeat
    Event=WaitWindowEvent()
    If Event=#PB_Event_Gadget
      Select EventGadget()
        Case #Increment
          CurrentVal=Val(GetGadgetText(#StringGadget))
          SetGadgetText(#StringGadget,Str(CurrentVal+1))
        Case #Random
          Flag=#PB_MessageRequester_YesNo
          Answer=MessageRequester("Randomize","Are you sure?",Flag)
          If Answer=#PB_MessageRequester_Yes
            SetGadgetText(#StringGadget,Str(Random(#MAXLONG)))
          EndIf
      EndSelect
    EndIf
  Until Event=#PB_Event_CloseWindow
  CloseWindow(0)
EndIf
