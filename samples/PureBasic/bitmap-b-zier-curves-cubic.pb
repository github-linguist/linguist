Procedure cubic_bezier(img, p1x, p1y, p2x, p2y, p3x, p3y, p4x, p4y, Color, n_seg)
  Protected i
  Protected.f t, t1, a, b, c, d
  Dim pts.POINT(n_seg)

  For i = 0 To n_seg
    t = i / n_seg
    t1 = 1.0 - t
    a = Pow(t1, 3)
    b = 3.0 * t * Pow(t1, 2)
    c = 3.0 * Pow(t, 2) * t1
    d = Pow(t, 3)
    pts(i)\x = a * p1x + b * p2x + c * p3x + d * p4x
    pts(i)\y = a * p1y + b * p2y + c * p3y + d * p4y
  Next

  StartDrawing(ImageOutput(img))
    FrontColor(Color)
    For i = 0 To n_seg - 1
      BresenhamLine(pts(i)\x, pts(i)\y, pts(i + 1)\x, pts(i + 1)\y) ;this calls the implementation of a draw_line routine
    Next
  StopDrawing()
EndProcedure

Define w, h, img
w = 200: h = 200: img = 1
CreateImage(img, w, h) ;img is internal id of the image

OpenWindow(0, 0, 0, w, h,"Bezier curve, cubic", #PB_Window_SystemMenu)
cubic_bezier(1, 160,10, 10,40, 30,160, 150,110, RGB(255, 255, 255), 20)
ImageGadget(0, 0, 0, w, h, ImageID(1))

Define event
Repeat
  event = WaitWindowEvent()
Until event = #PB_Event_CloseWindow
