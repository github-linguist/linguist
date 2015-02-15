Define MyWin.i, Event.i

MyWin = OpenWindow(#PB_Any, 412, 172, 402, 94, "PureBasic")

; Event loop
Repeat
   Event = WaitWindowEvent()
   Select Event
      Case #PB_Event_Gadget
         ; Handle any gadget events here
      Case #PB_Event_CloseWindow
         Break
   EndSelect
ForEver
