Procedure Triangle (X,Y, Length, N)
   If N = 0
      DrawText( Y,X, "*",#Blue)
   Else
      Triangle (X+Length,          Y, Length/2, N-1)
      Triangle (X,   Y+Length,        Length/2, N-1)
      Triangle (X+Length, Y+Length*2, Length/2, N-1)
   EndIf
EndProcedure


OpenWindow(0, 100, 100,700,500 ,"Sierpinski triangle",  #PB_Window_SystemMenu |1)
StartDrawing(WindowOutput(0))
   DrawingMode(#PB_2DDrawing_Transparent )
   Triangle(10,10,120,5)
StopDrawing()

Repeat
Until WaitWindowEvent()=#PB_Event_CloseWindow
End
