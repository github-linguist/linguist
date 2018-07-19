CreateImage(0,31,31)
StartDrawing(ImageOutput(0))
  For i=1 To 100
    Repeat
      x=Random(30)-15
      y=Random(30)-15
      R.f=Sqr(x*x+y*y)
    Until 10<=R And R<=15
    Plot(x+15,y+15,#Red)
  Next
StopDrawing()

Title$="PureBasic Plot"
Flags=#PB_Window_SystemMenu
OpenWindow(0,#PB_Ignore,#PB_Ignore,ImageWidth(0),ImageHeight(0),Title$,Flags)
ImageGadget(0,0,0,ImageWidth(0),ImageHeight(0),ImageID(0))
Repeat: Until WaitWindowEvent()=#PB_Event_CloseWindow
