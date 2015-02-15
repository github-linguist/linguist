Procedure quad_bezier(img, p1x, p1y, p2x, p2y, p3x, p3y, Color, n_seg)
  Protected i
  Protected.f T, t1, a, b, c, d
  Dim pts.POINT(n_seg)

  For i = 0 To n_seg
    T = i / n_seg
    t1 = 1.0 - T
    a = Pow(t1, 2)
    b = 2.0 * T * t1
    c = Pow(T, 2)
    pts(i)\x = a * p1x + b * p2x + c * p3x
    pts(i)\y = a * p1y + b * p2y + c * p3y
  Next

  StartDrawing(ImageOutput(img))
    FrontColor(Color)
    For i = 0 To n_seg - 1
      BresenhamLine(pts(i)\x, pts(i)\y, pts(i + 1)\x, pts(i + 1)\y)
    Next
  StopDrawing()
EndProcedure

Define w, h, img
w = 200: h = 200: img = 1
CreateImage(img, w, h) ;img is internal id of the image

OpenWindow(0, 0, 0, w, h,"Bezier curve, quadratic", #PB_Window_SystemMenu)
quad_bezier(1, 80,20, 130,80, 20,150, RGB(255, 255, 255), 20)
ImageGadget(0, 0, 0, w, h, ImageID(1))

Define event
Repeat
  event = WaitWindowEvent()
Until event = #PB_Event_CloseWindow
