Procedure BresenhamLine(x0 ,y0 ,x1 ,y1)
     If Abs(y1 - y0) > Abs(x1 - x0);
        steep =#True
        Swap x0, y0
        Swap x1, y1
     EndIf
     If x0 > x1
         Swap x0, x1
         Swap y0, y1
     EndIf
     deltax = x1 - x0
     deltay = Abs(y1 - y0)
     error = deltax / 2
     y = y0
     If y0 < y1
        ystep = 1
     Else
        ystep = -1
     EndIf
     For x = x0 To x1
         If steep
           Plot(y,x)
         Else
           Plot(x,y)
         EndIf
         error - deltay
         If error < 0
             y + ystep
             error + deltax
         EndIf
     Next
EndProcedure

#Window1   = 0
#Image1    = 0
#ImgGadget = 0
#width     = 300
#height    = 300

Define.i Event
Define.f Angle

If OpenWindow(#Window1, 0, 0, #width, #height, "Bresenham's Line PureBasic Example", #PB_Window_SystemMenu|#PB_Window_ScreenCentered)
   If CreateImage(#Image1, #width, #height)
      ImageGadget(#ImgGadget, 0, 0, #width, #height, ImageID(#Image1))
      StartDrawing(ImageOutput(#Image1))
      FillArea(0,0,-1,$FFFFFF) :FrontColor(0)
      While Angle < 2*#PI
        BresenhamLine(150,150,150+Cos(Angle)*120,150+Sin(Angle)*120)
        Angle + #PI/60
      Wend

      StopDrawing()
      SetGadgetState(#ImgGadget, ImageID(#Image1))
      Repeat
        Event = WaitWindowEvent()
      Until Event = #PB_Event_CloseWindow
   EndIf
EndIf
