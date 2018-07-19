Macro PlotB(x, y, Color, b)
  Plot(x, y, RGB(Red(Color) * (b), Green(Color) * (b), Blue(Color) * (b)))
EndMacro

Procedure.f fracPart(x.f)
  ProcedureReturn x - Int(x)
EndProcedure

Procedure.f invFracPart(x.f)
  ProcedureReturn 1.0 - fracPart(x)
EndProcedure

Procedure drawAntiAliasedLine(x1.f, y1.f, x2.f, y2.f, color)
  Protected.f dx, dy, xend, yend, grad, yf, xgap, ix1, iy1, ix2, iy2
  Protected x

  dx = x2 - x1
  dy = y2 - y1
  If Abs(dx) < Abs(dy)
    Swap x1, y1
    Swap x2, y2
    Swap dx, dy
  EndIf

  If x2 < x1
    Swap x1, x2
    Swap y1, y2
  EndIf

  grad = dy / dx

  ;handle first endpoint
  xend = Round(x1, #pb_round_nearest)
  yend = y1 + grad * (xend - x1)
  xgap = invFracPart(x1 + 0.5)
  ix1 = xend  ;this will be used in the MAIN loop
  iy1 = Int(yend)
  PlotB(ix1, iy1, color, invFracPart(yend) * xgap)
  PlotB(ix1, iy1 + 1, color, fracPart(yend) * xgap)
  yf = yend + grad ;first y-intersection for the MAIN loop

  ;handle second endpoint
  xend = Round(x2, #pb_round_nearest)
  yend = y2 + grad * (xend - x2)
  xgap = fracPart(x2 + 0.5)
  ix2 = xend  ;this will be used in the MAIN loop
  iy2 = Int(yend)
  PlotB(ix2, iy2, color, invFracPart(yend) * xgap)
  PlotB(ix2, iy2 + 1, color, fracPart(yend) * xgap)
  ;MAIN loop
  For x = ix1 + 1 To ix2 - 1
    PlotB(x, Int(yf), color, invFracPart(yf))
    PlotB(x, Int(yf) + 1, color, fracPart(yf))
    yf + grad
  Next
EndProcedure

Define w = 200, h = 200, img = 1
CreateImage(img, w, h) ;img is internal id of the image

OpenWindow(0, 0, 0, w, h,"Xiaolin Wu's line algorithm", #PB_Window_SystemMenu)

StartDrawing(ImageOutput(img))
  drawAntiAliasedLine(80,20, 130,80, RGB(255, 0, 0))
StopDrawing()

ImageGadget(0, 0, 0, w, h, ImageID(img))

Define event
Repeat
  event = WaitWindowEvent()
Until event = #PB_Event_CloseWindow
