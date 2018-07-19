#Win   = 0
#Demo1 = 0
#Demo2 = 1

If OpenWindow(#Win,50,50,200,60,"PureBasic",#PB_Window_SystemMenu)
  ;
  AddKeyboardShortcut(#Win,#PB_Shortcut_F1, #Demo1)
  AddKeyboardShortcut(#Win,#PB_Shortcut_F|#PB_Shortcut_Alt, #Demo2)
  ;
  Repeat
    WEvent = WaitWindowEvent()
    Select  WEvent
      Case #PB_Event_Menu
        Select EventMenu()
          Case #Demo1
            MessageRequester("Info", "You Pressed F1")

          Case #Demo2
            MessageRequester("Info", "You Pressed Alt-F")

        EndSelect
      Case #PB_Event_CloseWindow
        Break
    EndSelect
  ForEver
EndIf
