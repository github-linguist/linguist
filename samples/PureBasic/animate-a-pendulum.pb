Procedure handleError(x, msg.s)
  If Not x
    MessageRequester("Error", msg)
    End
  EndIf
EndProcedure

#ScreenW = 320
#ScreenH = 210
handleError(OpenWindow(0, 0, 0, #ScreenW, #ScreenH, "Animated Pendulum", #PB_Window_SystemMenu), "Can't open window.")
handleError(InitSprite(), "Can't setup sprite display.")
handleError(OpenWindowedScreen(WindowID(0), 0, 0, #ScreenW, #ScreenH, 0, 0, 0), "Can't open screen.")

Enumeration ;sprites
  #bob_spr
  #ceiling_spr
  #pivot_spr
EndEnumeration

TransparentSpriteColor(#PB_Default, RGB(255, 0, 255))
CreateSprite(#bob_spr, 32, 32)
StartDrawing(SpriteOutput(#bob_spr))
  Box(0, 0, 32, 32, RGB(255, 0, 255))
  Circle(16, 16, 15, RGB(253, 252, 3))
  DrawingMode(#PB_2DDrawing_Outlined)
  Circle(16, 16, 15, RGB(0, 0, 0))
StopDrawing()

CreateSprite(#pivot_spr, 10, 10)
StartDrawing(SpriteOutput(#pivot_spr))
  Box(0, 0, 10, 10, RGB(255, 0, 255))
  Circle(5, 5, 4, RGB(125, 125, 125))
  DrawingMode(#PB_2DDrawing_Outlined)
  Circle(5, 5, 4, RGB(0,0 , 0))
StopDrawing()

CreateSprite(#ceiling_spr,#ScreenW,2)
StartDrawing(SpriteOutput(#ceiling_spr))
  Box(0,0,SpriteWidth(#ceiling_spr), SpriteHeight(#ceiling_spr), RGB(126, 126, 126))
StopDrawing()

Structure pendulum
  length.d   ; meters
  constant.d ; -g/l
  gravity.d  ; m/s²
  angle.d    ; radians
  velocity.d ; m/s
EndStructure

Procedure initPendulum(*pendulum.pendulum, length.d = 1.0, gravity.d = 9.81, initialAngle.d = #PI / 2)
  With *pendulum
    \length = length
    \gravity = gravity
    \angle = initialAngle
    \constant = -gravity / length
    \velocity = 0.0
  EndWith
EndProcedure


Procedure updatePendulum(*pendulum.pendulum, deltaTime.d)
  deltaTime = deltaTime / 1000.0 ;ms
  Protected acceleration.d = *pendulum\constant * Sin(*pendulum\angle)
  *pendulum\velocity + acceleration * deltaTime
  *pendulum\angle + *pendulum\velocity * deltaTime
EndProcedure

Procedure drawBackground()
  ClearScreen(RGB(190,190,190))
  ;draw ceiling
  DisplaySprite(#ceiling_spr, 0, 47)
  ;draw pivot
  DisplayTransparentSprite(#pivot_spr, 154,43) ;origin in upper-left
EndProcedure

Procedure drawPendulum(*pendulum.pendulum)
  ;draw rod
  Protected x = *pendulum\length * 140 * Sin(*pendulum\angle) ;scale = 1 m/140 pixels
  Protected y = *pendulum\length * 140 * Cos(*pendulum\angle)
  StartDrawing(ScreenOutput())
    LineXY(154 + 5,43 + 5, 154 + 5 + x, 43 + 5 + y) ;draw from pivot-center to bob-center, adjusting for origins
  StopDrawing()

  ;draw bob
  DisplayTransparentSprite(#bob_spr, 154 + 5 - 16 + x, 43 + 5 - 16 + y) ;adj for origin in upper-left
EndProcedure

Define pendulum.pendulum, event
initPendulum(pendulum)
drawPendulum(pendulum)

AddWindowTimer(0, 1, 50)
Repeat
  event = WindowEvent()
  Select event
    Case #pb_event_timer
      drawBackground()
      Select EventTimer()
        Case 1
          updatePendulum(pendulum, 50)
          drawPendulum(pendulum)
      EndSelect
      FlipBuffers()
    Case #PB_Event_CloseWindow
      Break
  EndSelect
ForEver
