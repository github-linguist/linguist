Procedure ImageGrayout(image)
  w = ImageWidth(image)
  h = ImageHeight(image)
  StartDrawing(ImageOutput(image))
  For x = 0 To w - 1
    For y = 0 To h - 1
      color = Point(x, y)
      r    = color & $ff
      g    = color >> 8 & $ff
      b    = color >> 16 & $ff
      gray = 0.2126*r + 0.7152*g + 0.0722*b
      Plot(x, y, gray + gray << 8 + gray << 16)
    Next
  Next
  StopDrawing()
EndProcedure
