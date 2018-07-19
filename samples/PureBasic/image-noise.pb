#filter=0.2       ; Filter parameter for the FPS-calculation
#UpdateFreq=100   ; How often to update the FPS-display

OpenWindow(0,400,300,320,240,"PureBasic")
Define w=WindowWidth(0), h=WindowHeight(0)
Define x, y, T, TOld, FloatingMedium.f, cnt
InitSprite()
OpenWindowedScreen(WindowID(0),0,0,w,h,1,0,0,#PB_Screen_NoSynchronization)
Repeat
  StartDrawing(ScreenOutput())
  For y=0 To h-1
    For x=0 To w-1
      If Random(1)
        Plot(x,y,#Black)
      Else
        Plot(x,y,#White)
      EndIf
    Next
  Next
  StopDrawing()
  FlipBuffers()
  cnt+1
  If cnt>=#UpdateFreq
    cnt =0
    TOld=T
    T   =ElapsedMilliseconds()
    FloatingMedium*(1-#filter)+1000*#filter/(T-TOld)
    SetWindowTitle(0,"PureBasic: "+StrF(#UpdateFreq*FloatingMedium,2)+" FPS")
    Repeat ; Handle all events
      Event=WindowEvent()
      If Event=#PB_Event_CloseWindow
        End
      EndIf
    Until Not Event
  EndIf
ForEver
