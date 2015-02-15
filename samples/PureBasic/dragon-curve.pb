#SqRt2 = 1.4142136
#SizeH = 800: #SizeV = 550
Global angle.d, px, py, imageNum

Procedure turn(degrees.d)
  angle + degrees * #PI / 180
EndProcedure

Procedure forward(length.d)
  Protected w = Cos(angle) * length
  Protected h = Sin(angle) * length
  LineXY(px, py, px + w, py + h, RGB(255,255,255))
  px + w: py + h
EndProcedure

Procedure dragon(length.d, split, d.d)
  If split = 0
    forward(length)
  Else
    turn(d * 45)
    dragon(length / #SqRt2, split - 1, 1)
    turn(-d * 90)
    dragon(length / #SqRt2, split - 1, -1)
    turn(d * 45)
  EndIf
EndProcedure

OpenWindow(0, 0, 0, #SizeH, #SizeV, "DragonCurve", #PB_Window_SystemMenu)
imageNum = CreateImage(#PB_Any, #SizeH, #SizeV, 32)
ImageGadget(0, 0, 0, 0, 0, ImageID(imageNum))

angle = 0: px = 185: py = 190
If StartDrawing(ImageOutput(imageNum))
    dragon(400, 15, 1)
  StopDrawing()
  SetGadgetState(0, ImageID(imageNum))
EndIf

Repeat: Until WaitWindowEvent(10) = #PB_Event_CloseWindow
