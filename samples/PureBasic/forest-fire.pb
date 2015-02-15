; Some systems reports high CPU-load while running this code.
; This may likely be due to the graphic driver used in the
; 2D-function Plot().
; If experiencing this problem, please reduce the #Width & #Height
; or activate the parameter #UnLoadCPU below with a parameter 1 or 2.
;
; This code should work with the demo version of PureBasic on both PC & Linux

; General parameters for the world
#f    = 1e-6
#p    = 1e-2
#SeedATree  = 0.005
#Width      = 400
#Height     = 400

; Setting up colours
#Fire       = $080CF7
#BackGround = $BFD5D3
#YoungTree  = $00E300
#NormalTree = $00AC00
#MatureTree = $009500
#OldTree    = $007600
#Black      = $000000

; Depending on your hardware, use this to control the speed/CPU-load.
; 0 = No load reduction
; 1 = Only active about every second frame
; 2 = '1' & release the CPU after each horizontal line.
#UnLoadCPU  = 0

Enumeration
  #Empty  =0
  #Ignited
  #Burning
  #Tree
  #Old=#Tree+20
EndEnumeration

Global Dim Forest.i(#Width, #Height)
Global Title$="Forest fire in PureBasic"
Global Cnt

Macro Rnd()
  (Random(2147483647)/2147483647.0)
EndMacro

Procedure Limit(n, min, max)
  If n<min
    n=min
  ElseIf n>max
    n=max
  EndIf
  ProcedureReturn n
EndProcedure

Procedure SpreadFire(x,y)
  Protected cnt=0, i, j
  For i=Limit(x-1, 0, #Width) To Limit(x+1, 0, #Width)
    For j=Limit(y-1, 0, #Height) To Limit(y+1, 0, #Height)
      If Forest(i,j)>=#Tree
        Forest(i,j)=#Ignited
      EndIf
    Next
  Next
EndProcedure

Procedure InitMap()
  Protected x, y, type
  For y=1 To #Height
    For x=1 To #Width
      If Rnd()<=#SeedATree
        type=#Tree
      Else
        type=#Empty
      EndIf
      Forest(x,y)=type
    Next
  Next
EndProcedure

Procedure UpdateMap()
  Protected x, y
  For y=1 To #Height
    For x=1 To #Width
      Select Forest(x,y)
        Case #Burning
          Forest(x,y)=#Empty
          SpreadFire(x,y)
        Case #Ignited
          Forest(x,y)=#Burning
        Case #Empty
          If Rnd()<=#p
            Forest(x,y)=#Tree
          EndIf
        Default
          If Rnd()<=#f
            Forest(x,y)=#Burning
          Else
            Forest(x,y)+1
          EndIf
      EndSelect
    Next
  Next
EndProcedure

Procedure PresentMap()
  Protected x, y, c
  cnt+1
  SetWindowTitle(0,Title$+", time frame="+Str(cnt))
  StartDrawing(ImageOutput(1))
  For y=0 To OutputHeight()-1
    For x=0 To OutputWidth()-1
      Select Forest(x,y)
        Case #Empty
          c=#BackGround
        Case #Burning, #Ignited
          c=#Fire
        Default
          If Forest(x,y)<#Tree+#Old
            c=#YoungTree
          ElseIf Forest(x,y)<#Tree+2*#Old
            c=#NormalTree
          ElseIf Forest(x,y)<#Tree+3*#Old
            c=#MatureTree
          ElseIf Forest(x,y)<#Tree+4*#Old
            c=#OldTree
          Else ; Tree died of old age
            Forest(x,y)=#Empty
            c=#Black
          EndIf
      EndSelect
      Plot(x,y,c)
    Next
    CompilerIf #UnLoadCPU>1
      Delay(1)
    CompilerEndIf
  Next
  StopDrawing()
  ImageGadget(1, 0, 0, #Width, #Height, ImageID(1))
EndProcedure

If OpenWindow(0, 10, 30, #Width, #Height, Title$, #PB_Window_MinimizeGadget)
  SmartWindowRefresh(0, 1)
  If CreateImage(1, #Width, #Height)
    Define Event, freq
    If ExamineDesktops() And DesktopFrequency(0)
      freq=DesktopFrequency(0)
    Else
      freq=60
    EndIf
    AddWindowTimer(0,0,5000/freq)
    InitMap()
    Repeat
      Event = WaitWindowEvent()
      Select Event
        Case #PB_Event_CloseWindow
          End
        Case #PB_Event_Timer
          CompilerIf #UnLoadCPU>0
            Delay(25)
          CompilerEndIf
          UpdateMap()
          PresentMap()
      EndSelect
    ForEver
  EndIf
EndIf
