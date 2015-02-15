Procedure Draw_a_Cuboid(Window, X,Y,Z)
  w=WindowWidth(Window)
  h=WindowHeight(Window)
  diag.f=1.9
  If Not (w And h): ProcedureReturn: EndIf
  xscale.f = w/(x+z/diag)*0.98
  yscale.f = h/(y+z/diag)*0.98
  If xscale<yscale
    Scale.f = xscale
  Else
    Scale = yscale
  EndIf
  x*Scale: Y*Scale: Z*Scale
  CreateImage(0,w,h)
  If StartDrawing(ImageOutput(0))
    c= RGB(250, 40, 5)

    ;- Calculate the cones in the Cuboid
    xk = w/50     : yk = h/50
    x0 = Z/2 + xk : y0 = yk
    x1 = x0 + X   : y1 = y0
    x2 = xk       : y2 = y0 + Z/2
    x3 = x2 + X   : y3 = y2
    x4 = x2       : y4 = y2 + Y
    x5 = x4 + X   : y5 = y4
    x6 = x5 + Z/2 : y6 = y5 - Z/2

    ;- Draw it
    LineXY(x0,y0,x1,y1,c)
    LineXY(x0,y0,x2,y2,c)
    LineXY(x2,y2,x3,y3,c)
    LineXY(x1,y1,x3,y3,c)
    LineXY(x2,y2,x4,y4,c)
    LineXY(x4,y4,x5,y5,c)
    LineXY(x5,y5,x4,y4,c)
    LineXY(x5,y5,x6,y6,c)
    LineXY(x5,y5,x3,y3,c)
    LineXY(x6,y6,x1,y1,c)

    ;- Fill the areas
    FillArea(x,y,-1,RGB(255, 0, 0))
    FillArea(x,y-z/2,-1,RGB(0, 0, 255))
    FillArea(x+z/2,y,-1,RGB(0, 255, 0))
    StopDrawing()
  EndIf
  ;- Update the graphic
  ImageGadget(0,0,0,w,h,ImageID(0))
EndProcedure

#WFlags = #PB_Window_SystemMenu|#PB_Window_SizeGadget
#title  = "PureBasic Cuboid"
MyWin = OpenWindow(#PB_Any, 0, 0, 200, 250, #title, #WFlags)

Repeat
  WEvent = WaitWindowEvent()
  If WEvent = #PB_Event_SizeWindow
    Draw_a_Cuboid(MyWin, 2, 3, 4)
  EndIf
Until WEvent = #PB_Event_CloseWindow

;-  Save the image?
UsePNGImageEncoder()
respons = MessageRequester("Question","Save the image?",#PB_MessageRequester_YesNo)
If respons=#PB_MessageRequester_Yes
  SaveImage(0, SaveFileRequester("","","",0),#PB_ImagePlugin_PNG,9)
EndIf
