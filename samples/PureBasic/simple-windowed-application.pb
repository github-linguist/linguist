Global Window_0
Global Window_0_Text_0
Global Window_0_Button_1
Global Clicks, txt$

Procedure OpenWindow_Window_0()
  Protected flags=#PB_Window_SystemMenu|#PB_Window_SizeGadget|#PB_Window_TitleBar|#PB_Window_WindowCentered
  Window_0 = OpenWindow(#PB_Any, 408, 104, 280, 45, "Simple windowed application", flags)
  If Window_0
    SmartWindowRefresh(Window_0, #True)
    Window_0_Text_0 = TextGadget(#PB_Any, 5, 5, 165, 20, "There have been no clicks yet")
    Window_0_Button_1 = ButtonGadget(#PB_Any, 190, 10, 85, 30, "Click me")
  EndIf
EndProcedure

OpenWindow_Window_0()

Repeat
  Select WaitWindowEvent()
    Case #PB_Event_Gadget
      Select EventGadget()
        Case Window_0_Text_0
        Case Window_0_Button_1
          Clicks+1
          txt$="You Clicked "+Str(Clicks)+" time"
          If Clicks>1: txt$+"s": EndIf
          SetGadgetText(Window_0_Text_0,txt$)
      EndSelect
    Case #PB_Event_CloseWindow
       End
  EndSelect
ForEver
