Function CreatePicture(width As Integer, height As Integer, depth As Integer) As Picture
  'It is not possible to create a Picture object with dimensions without initializing it
  Return New Picture(width, height, depth)
End Function

Sub FillPicture(ByRef p As Picture, FillColor As Color)
  p.Graphics.ForeColor = FillColor
  p.Graphics.FillRect(0, 0, p.Width, p.Height)
End Sub

Function GetPixelColor(p As Picture, x As Integer, y As Integer) As Color
  Return p.Graphics.Pixel(x, y)
End Function

Sub SetPixelColor(p As Picture, x As Integer, y As Integer, pColor As Color)
  p.Graphics.Pixel(x, y) = pColor
End Sub
