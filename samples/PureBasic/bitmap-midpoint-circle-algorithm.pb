Procedure rasterCircle(cx, cy, r, Color)
  ;circle must lie completely within the image boundaries
  Protected f= 1 - r
  Protected ddF_X, ddF_Y = -2 * r
  Protected x, y = r

  Plot(cx, cy + r, Color)
  Plot(cx, cy - r, Color)
  Plot(cx + r, cy, Color)
  Plot(cx - r, cy, Color)
  While x < y
    If f >= 0
      y - 1
      ddF_Y + 2
      f + ddF_Y
    EndIf
    x + 1
    ddF_X + 2
    f + ddF_X + 1
    Plot(cx + x, cy + y, Color)
    Plot(cx - x, cy + y, Color)
    Plot(cx + x, cy - y, Color)
    Plot(cx - x, cy - y, Color)
    Plot(cx + y, cy + x, Color)
    Plot(cx - y, cy + x, Color)
    Plot(cx + y, cy - x, Color)
    Plot(cx - y, cy - x, Color)
  Wend
EndProcedure

OpenWindow(0, 0, 0, 100, 100, "MidPoint Circle Algorithm", #PB_Window_SystemMenu)
CreateImage(0, 100, 100, 32)
StartDrawing(ImageOutput(0))
  Box(0, 0, 100, 100, RGB(0, 0, 0))
  rasterCircle(25, 25, 20, RGB(255, 255, 255))
  rasterCircle(50, 50, 40, RGB(255, 0, 0))
StopDrawing()
ImageGadget(0, 0, 0, 0, 0, ImageID(0))

Repeat: Until WaitWindowEvent() = #PB_Event_CloseWindow
