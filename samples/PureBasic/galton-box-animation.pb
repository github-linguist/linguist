Global pegRadius, pegSize, pegSize2, height, width, delay, histogramSize, ball

Procedure eventLoop()
  Protected event
  Repeat
    event = WindowEvent()
    If event = #PB_Event_CloseWindow
      End
    EndIf
  Until event = 0
EndProcedure

Procedure animate_actual(x1, y1, x2, y2, steps)
  Protected x.f, y.f, xstep.f, ystep.f, i, lastX.f, lastY.f
  x = x1
  y = y1
  xstep = (x2 - x1)/steps
  ystep = (y2 - y1)/steps
  For i = 1 To steps
    lastX = x
    lastY = y
    StartDrawing(CanvasOutput(0))
      DrawingMode(#PB_2DDrawing_XOr)
      Circle(x, y, pegRadius, RGB(0, 255, 255))
    StopDrawing()
    eventLoop()
    Delay(delay)      ; wait in ms
    StartDrawing(CanvasOutput(0))
      DrawingMode(#PB_2DDrawing_XOr)
      Circle(x, y, pegRadius, RGB(0, 255, 255))
    StopDrawing()
    eventLoop()
    x + xstep
    y + ystep
  Next
EndProcedure

Procedure draw_ball(xpos, ypos)
  Static Dim ballcounts(0) ;tally drop positions
  If xpos > ArraySize(ballcounts())
    Redim ballcounts(xpos)
  EndIf
  ballcounts(xpos) + 1
  animate_actual(xpos, ypos, xpos, height - ballcounts(xpos) * pegSize, 20)
  StartDrawing(CanvasOutput(0))
    Circle(xpos, height - ballcounts(xpos) * pegSize, pegRadius, RGB(255, 0, 0))
  StopDrawing()
  eventLoop()
  If ballcounts(xpos) <= histogramSize
    ProcedureReturn 1
  EndIf
  SetWindowTitle(0, "Ended after " + Str(ball) + " balls") ;histogramSize exceeded
EndProcedure

Procedure animate(x1, y1, x2, y2)
  animate_actual(x1, y1, x2, y1, 4)
  animate_actual(x2, y1, x2, y2, 10)
EndProcedure

Procedure galton(pegRows)
  ;drop a ball into the galton box
  Protected xpos, ypos, i, oldX, oldY

  oldX = width / 2 - pegSize / 2
  xpos = oldX
  oldY = pegSize
  ypos = oldY
  animate_actual(oldX, 0, xpos, ypos, 10)
  For i = 1 To pegRows
    If Random(1)
      xpos + pegSize
    Else
      xpos - pegSize
    EndIf
    ypos + pegSize2
    animate(oldX, oldY, xpos, ypos)
    oldX = xpos
    oldY = ypos
  Next

  ProcedureReturn draw_ball(xpos, ypos)
EndProcedure

Procedure setup_window(numRows, ballCount)
  ;Draw numRows levels of pegs
  Protected xpos, ypos, i, j

  width = (2 * numRows + 2) * pegSize
  histogramSize = (ballCount + 2) / 3
  If histogramSize > 500 / pegSize: histogramSize = 500 / pegSize: EndIf
  height = width + histogramSize * pegSize
  OpenWindow(0, 0, 0, width, height, "Galton box animation", #PB_Window_SystemMenu)
  CanvasGadget(0, 0, 0, width, height)

  StartDrawing(CanvasOutput(0))
    Box(0, 0, width, height, RGB($EB, $EB, $EB))
    For i = 1 To numRows
      ypos = i * pegSize2
      xpos = width / 2 - (i - 1) * pegSize - pegSize / 2
      For j = 1 To i
        Circle(xpos, ypos, pegRadius, RGB(0, 0, 255))
        xpos + pegSize2
      Next
    Next
    For i = 1 To numRows
      Line((numRows - i + 1) * pegSize2 - pegSize / 2, width - pegSize, 1, histogramSize * pegSize, 0)
    Next
  StopDrawing()
EndProcedure

;based on the galton box simulation from Unicon book
Define pegRows = 10, ballCount
pegRadius = 4
pegSize = pegRadius * 2 + 1
pegSize2 = pegSize * 2
delay = 2                      ; ms delay

Repeat
  ballCount = Val(InputRequester("Galton box simulator","How many balls to drop?", "100"))
Until ballCount > 0

setup_window(pegRows, ballCount)
eventLoop()
For ball = 1 To ballCount
  If Not galton(pegRows): Break: EndIf
Next
Repeat: eventLoop(): ForEver
