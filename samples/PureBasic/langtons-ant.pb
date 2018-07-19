#White = $FFFFFF
#Black = 0
#planeHeight = 100
#planeWidth = 100
#canvasID = 0
#windowID = 0
OpenWindow(#windowID, 0, 0, 150, 150, "Langton's ant", #PB_Window_SystemMenu | #PB_Window_ScreenCentered)
CanvasGadget(#canvasID, 25, 25, #planeWidth, #planeHeight)
StartDrawing(CanvasOutput(#canvasID))
  Box(0, 0, #planeWidth, #planeHeight, #White)
StopDrawing()

Define event, quit, ant.POINT, antDirection, antSteps

ant\x = #planeHeight / 2
ant\y = #planeWidth / 2
Repeat
  Repeat
    event = WindowEvent()
    If event = #PB_Event_CloseWindow
      quit = 1
      event = 0
    EndIf
  Until event = 0

  StartDrawing(CanvasOutput(#canvasID))
    Select Point(ant\x, ant\y)
      Case #Black
        Plot(ant\x, ant\y, #White)
        antDirection = (antDirection + 1) % 4 ;turn left
      Case #White
        Plot(ant\x, ant\y, #Black)
        antDirection = (antDirection - 1 + 4) % 4 ;turn right
    EndSelect
  StopDrawing()

  Select antDirection
    Case 0 ;up
      ant\y - 1
    Case 1 ;left
      ant\x - 1
    Case 2 ;down
      ant\y + 1
    Case 3 ;right
      ant\x + 1
  EndSelect
  antSteps + 1

  If ant\x < 0 Or ant\x >= #planeWidth Or ant\y < 0 Or ant\y >= #planeHeight
    MessageRequester("Langton's ant status", "Out of bounds after " + Str(antSteps) + " steps.")
    quit = 1
  EndIf

  Delay(10) ;control animation speed and avoid hogging CPU
Until quit = 1
