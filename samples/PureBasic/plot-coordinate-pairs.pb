Structure PlotData
  x.i
  y.f
EndStructure

Global i, x, y.f, max_x, max_y, min_x = #MAXLONG, min_y = Infinity()
Define count = (?serie_y - ?serie_x) / SizeOf(Integer) - 1
Global Dim MyData.PlotData(count)

Restore serie_x
For i = 0 To count
  Read.i x
  MyData(i)\x = x
  If x > max_x: max_x = x:  EndIf
  If x < min_x: min_x = x:  EndIf
Next
Restore serie_y
For i = 0 To count
  Read.f  y
  MyData(i)\y = y
  If y > max_y: max_y = y:  EndIf
  If y < min_y: min_y = y:  EndIf
Next

Procedure UpdatePlot(Win, w, h)
  Static gblm = 20, gtrm = 5 ;graph's bottom-left and top-right margin

  Protected count = ArraySize(MyData())
  If w > gblm And h > gblm And count > 0
    SetWindowTitle(Win, "PureBasic Plot " + Str(w) + "x" + Str(h))
    Protected gw = w - gblm, gh = h - gblm ;graph's width and height
    Protected i, yf.f, xf.f
    yf = (gh - gtrm) / max_y
    xf = (gw - gtrm) / max_x

    CreateImage(0, w, h)
    Protected OutputID = ImageOutput(0)
    StartDrawing(OutputID)
      DrawingMode(#PB_2DDrawing_Transparent)
      ;- Draw grid
      For i = 0 To count
        y = gh - max_y * i / count * yf
        LineXY(gblm, y, w - gtrm, y, $467E3E)
        ; Y-scale
        DrawText(1, y - 5, RSet(StrD(i / count * max_y, 1), 5))
        x = gblm + max_x * i / count * xf
        y = gh
        ; X-Scale
        LineXY(x, y, x, gtrm, $467E3E)
        If i: DrawText(x - 5, y + 2, Str(i)): EndIf
      Next

      ;- Draw curve
      Protected ox = gblm, oy = gh, x, y
      For i = 0 To count
        x = gblm + MyData(i)\x * xf
        y = gh - MyData(i)\y * yf
        LineXY(ox, oy, x, y, $0133EE)
        ox = x: oy = y
      Next
    StopDrawing()
    ImageGadget(0, 0, 0, w, h, ImageID(0))
  EndIf
EndProcedure

Define Win = OpenWindow(#PB_Any, 0, 0, 600, 400,"", #PB_Window_SystemMenu | #PB_Window_SizeGadget)
If Win
  SmartWindowRefresh(Win, 1)
  UpdatePlot(Win, WindowWidth(Win), WindowHeight(Win))
  Repeat
    Define event = WaitWindowEvent()
    Select event
      Case #PB_Event_SizeWindow
        UpdatePlot(Win, WindowWidth(Win), WindowHeight(Win))
    EndSelect
  Until event = #PB_Event_CloseWindow

  ; Save the plot if the user wants to
  If MessageRequester("Question", "Save it?", #PB_MessageRequester_YesNo) = #PB_MessageRequester_Yes
    Define File$=SaveFileRequester("Save as", "PB.png", "PNG (*.png)|*.png", 0)
    UsePNGImageEncoder()
    SaveImage(0, File$, #PB_ImagePlugin_PNG)
  EndIf
EndIf

DataSection
  serie_x:
  Data.i 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
  serie_y:
  Data.f 2.7, 2.8, 31.4, 38.1, 58.0, 76.2, 100.5, 130.0, 149.3, 180.0
EndDataSection
