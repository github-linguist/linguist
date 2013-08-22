#WindowTitle = "Hello World!"
#WindowWidth = 300
#WindowHeight = 100

Procedure Main()
    TextGadget(#PB_Any, 0, WindowHeight(Main)/2, WindowWidth(Main), WindowHeight(Height)/2, #WindowTitle, #PB_Text_Center)
EndProcedure

Procedure Init()
    If OpenWindow(0, 0, 0, #WindowWidth, #WindowHeight, #WindowTitle, #PB_Window_ScreenCentered|#PB_Window_MaximizeGadget)
        Main()
        Repeat
            WindowEvent = WaitWindowEvent()
        Until WindowEvent = #PB_Event_CloseWindow
    EndIf
EndProcedure

Init()