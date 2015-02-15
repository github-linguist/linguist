OpenWindow(0,0,0,500,100,"Hello World!",#PB_Window_ScreenCentered|#PB_Window_SystemMenu)

text$ = "Hello World! "
direction = 1

LoadFont(0,"",60)
ButtonGadget(0,2,2,496,96,text$) : SetGadgetFont(0,FontID(0))

Repeat
  event = WaitWindowEvent(50)
  Select event
    Case #PB_Event_Gadget
      If EventGadget() = 0
        direction*-1
      EndIf
    Case #PB_Event_CloseWindow
      End
  EndSelect

  If ElapsedMilliseconds()-tick > 400
    offset+direction
    If offset > Len(text$)-1
      offset = 0
    ElseIf offset < 0
      offset = Len(text$)-1
    EndIf
    SetGadgetText(0,Mid(text$,offset+1)+Left(text$,offset))
    tick = ElapsedMilliseconds()
  EndIf
ForEver
