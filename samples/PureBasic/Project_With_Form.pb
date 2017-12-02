; Sample project for GitHub's Linguist:
; https://github.com/github/linguist/tree/master/samples/PureBasic
; PureBasic 5.61 | 2017/12/02 | by Tristano Ajmone
; Public domain.

XIncludeFile "Project_With_Form.pbf" ; Include form file

OpenMainWindow()

Procedure OkButtonEvent(Event)
  MessageRequester("Happy Button", "Thanks for clicking me, I was feeling lonely!")
EndProcedure


Repeat
  Event = WaitWindowEvent()
  
  Select EventWindow()
    Case MainWindow
      MainWindow_Events(Event)
  EndSelect
  
Until Event = #PB_Event_CloseWindow ; Quit on window close

