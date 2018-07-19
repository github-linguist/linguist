#Spread_Ang     = 35
#Scaling_Factor = 0.75
#Deg_to_Rad = #PI / 180
#SizeH = 500
#SizeV = 375
#Init_Size = 100

Procedure drawTree(x1, y1, Size, theta, depth)
  Protected x2 = x1 + Cos(theta * #Deg_to_Rad) * Size, y2 = y1 + Sin(theta * #Deg_to_Rad) * Size
  LineXY(x1, y1, x2, y2, RGB(255, 255, 255))
  If depth <= 0
    ProcedureReturn
  EndIf
  ;draw left branch
  drawTree(x2, y2, Size * #Scaling_Factor, theta - #Spread_Ang, depth - 1)
  ;draw right branch
  drawTree(x2, y2, Size * #Scaling_Factor, theta + #Spread_Ang, depth - 1)
EndProcedure


OpenWindow(0, 0, 0, #SizeH, #SizeV, "Fractal Tree", #PB_Window_SystemMenu)
Define fractal = CreateImage(#PB_Any, #SizeH, #SizeV, 32)
ImageGadget(0, 0, 0, 0, 0, ImageID(fractal))

If StartDrawing(ImageOutput(fractal))
    drawTree(#SizeH / 2, #SizeV, #Init_Size, -90, 9)
  StopDrawing()
  SetGadgetState(0, ImageID(fractal))
EndIf

Repeat: Until WaitWindowEvent(10) = #PB_Event_CloseWindow
