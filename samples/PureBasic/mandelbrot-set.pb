EnableExplicit

#Window1   = 0
#Image1    = 0
#ImgGadget = 0

#max_iteration =  64
#width         = 800
#height        = 600
Define.d x0 ,y0 ,xtemp ,cr, ci
Define.i i, n, x, y ,Event ,color

Dim Color.l (255)
For n = 0 To 63
  Color(   0 + n ) = RGB(  n*4+128, 4 * n, 0 )
  Color(  64 + n ) = RGB(  64, 255, 4 * n )
  Color( 128 + n ) = RGB(  64, 255 - 4 * n , 255 )
  Color( 192 + n ) = RGB(  64, 0, 255 - 4 * n )
Next

If OpenWindow(#Window1, 0, 0, #width, #height, "'Mandelbrot set' PureBasic Example", #PB_Window_SystemMenu )
    If CreateImage(#Image1, #width, #height)
       ImageGadget(#ImgGadget, 0, 0, #width, #height, ImageID(#Image1))
       For y.i = 1 To #height -1
         StartDrawing(ImageOutput(#Image1))
         For x.i = 1 To  #width -1
           x0 = 0
           y0 = 0;
           cr = (x / #width)*2.5 -2
           ci = (y / #height)*2.5 -1.25
           i = 0
           While  (x0*x0 + y0*y0 <= 4.0) And i < #max_iteration
             i +1
             xtemp = x0*x0 - y0*y0 + cr
             y0    = 2*x0*y0 + ci
             x0    = xtemp
           Wend
           If i >= #max_iteration
              Plot(x, y,  0 )
           Else
              Plot(x, y,  Color(i & 255))
           EndIf

         Next
         StopDrawing()
         SetGadgetState(#ImgGadget, ImageID(#Image1))
         Repeat
           Event = WindowEvent()
           If Event = #PB_Event_CloseWindow
             End
           EndIf
         Until Event = 0
       Next
    EndIf
    Repeat
      Event = WaitWindowEvent()
    Until Event = #PB_Event_CloseWindow
  EndIf
