OpenWindow (0, 10, 10, 150, 40, "Event Demo")
ButtonGadget (1, 10, 10, 35, 20, "Quit")

Repeat

   Event = WaitWindowEvent()

   If  Event = #PB_Event_Gadget And EventGadget() = 1
      End
   EndIf

ForEver
