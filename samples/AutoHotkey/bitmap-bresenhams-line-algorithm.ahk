Blue := Color(0,0,255)
White := Color(255,255,255)
Bitmap := Bitmap(100,100,Blue) ;create a 100*100 blue bitmap
Line(Bitmap,White,5,10,60,80) ;draw a white line from (5,10) to (60,80)
Bitmap.Write("Line.ppm") ;write the bitmap to file

Line(ByRef Bitmap,ByRef Color,PosX1,PosY1,PosX2,PosY2)
{
 DeltaX := Abs(PosX2 - PosX1), DeltaY := -Abs(PosY2 - PosY1) ;calculate deltas
 StepX := ((PosX1 < PosX2) ? 1 : -1), StepY := ((PosY1 < PosY2) ? 1 : -1) ;calculate steps
 ErrorValue := DeltaX + DeltaY ;calculate error value
 Loop ;loop over the pixel values
 {
  Bitmap[PosX1,PosX2] := Color
  If (PosX1 = PosX2 && PosY1 = PosY2)
   Break
  Temp1 := ErrorValue << 1, ((Temp1 > DeltaY) ? (ErrorValue += DeltaY, PosX1 += StepX) : ""), ((Temp1 < DeltaX) ? (ErrorValue += DeltaX, PosY1 += StepY) : "") ;move forward
 }
}
