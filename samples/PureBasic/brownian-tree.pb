#Window1   = 0
#Image1    = 0
#ImgGadget = 0

#NUM_PARTICLES = 3000
#width         = 200
#height        = 200
#xmax          = #width  -3
#ymax          = #height -3
Define.i Event ,i ,x,y

If OpenWindow(#Window1, 0, 0, #width, #height, "Brownian Tree PureBasic Example", #PB_Window_SystemMenu )
   If CreateImage(#Image1, #width, #height)
      ImageGadget(#ImgGadget, 0, 0, #width, #height, ImageID(#Image1))
      StartDrawing(ImageOutput(#Image1))
      FrontColor($FFFFFF)
      Plot( Random(#xmax) , Random(#ymax ))
      StopDrawing()
      SetGadgetState(#ImgGadget, ImageID(#Image1))
      For i = 1 To #NUM_PARTICLES
          x = Random(#xmax)+1 : y = Random (#ymax)+1
          StartDrawing(ImageOutput(#Image1))
          While Point(x+1, y+1) + Point(x, y+1)+Point(x+1, y)+Point(x-1, y-1)+Point(x-1, y)+Point(x, y-1) = 0
              x = x + (Random(2)-1) : y = y + (Random(2)-1)
              If x < 1 Or x > #xmax Or y < 1 Or y > #ymax
                  x = Random(#xmax)+1 : y = Random (#ymax)+1
              EndIf
          Wend
          Plot(x,y)
          StopDrawing()
          SetGadgetState(#ImgGadget, ImageID(#Image1))
      Next

   EndIf

    Repeat
      Event = WaitWindowEvent()
    Until Event = #PB_Event_CloseWindow
EndIf
