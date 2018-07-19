If OpenWindow(0, 0, 0, 5, 5, "", #PB_Window_Maximize + #PB_Window_Invisible)
  maxX = WindowWidth(0)
  maxY = WindowHeight(0)
  CloseWindow(0)
  MessageRequester("Result", "Maximum Window Width: " + Str(maxX) + ", Maximum Window Height: " + Str(maxY))
EndIf
