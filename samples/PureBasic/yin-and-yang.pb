Procedure Yin_And_Yang(x, y, radius)
  DrawingMode(#PB_2DDrawing_Outlined)
  Circle(x, y, 2 * radius, #Black)               ;outer circle
  DrawingMode(#PB_2DDrawing_Default)
  LineXY(x, y - 2 * radius, x, y + 2 * radius, #Black)
  FillArea(x + 1, y, #Black, #Black)
  Circle(x, y - radius, radius - 1, #White)
  Circle(x, y + radius, radius - 1, #Black)
  Circle(x, y - radius, radius / 3, #Black)       ;small contrasting inner circles
  Circle(x, y + radius, radius / 3, #White)
EndProcedure

If CreateImage(0, 700, 700) And StartDrawing(ImageOutput(0))
    FillArea(1, 1, -1, #White)
    Yin_And_Yang(105, 105, 50)
    Yin_And_Yang(400, 400, 148)
  StopDrawing()
  ;
  UsePNGImageEncoder()
  path$ = SaveFileRequester("Save image", "Yin And yang.png", "*.png", 0)
  If path$ <> "": SaveImage(0, path$, #PB_ImagePlugin_PNG, 0, 2): EndIf
EndIf
