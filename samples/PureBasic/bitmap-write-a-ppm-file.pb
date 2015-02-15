Procedure SaveImageAsPPM(Image, file$, Binary = 1)
  ; Author Roger Rösch (Nickname Macros)
  IDFiIe = CreateFile(#PB_Any, file$)
  If IDFiIe
    If StartDrawing(ImageOutput(Image))
      WriteStringN(IDFiIe, "P" + Str(3 + 3*Binary))
      WriteStringN(IDFiIe, "#Created with PureBasic using a Function created from Macros for Rosettacode.org ")
      width  = ImageWidth(Image)
      height = ImageHeight(Image)
      WriteStringN(IDFiIe, Str(width) + " " + Str(height))
      WriteStringN(IDFiIe, "255")
      If Binary = 0
        For y = 0 To height - 1
          For x = 0 To width - 1
            color = Point(x, y)
            WriteString(IDFiIe, Str(Red(color)) + " " + Str(Green(color)) + " " + Str(Blue(color)) + "  ")
          Next
          WriteStringN(IDFiIe, "")
        Next
      Else  ; Save in Binary Format
        For y = 0 To height - 1
          For x = 0 To width - 1
            color = Point(x, y)
            WriteByte(IDFiIe, Red(color))
            WriteByte(IDFiIe, Green(color))
            WriteByte(IDFiIe, Blue(color))
          Next
        Next
      EndIf
      StopDrawing()
    EndIf
    CloseFile(IDFiIe)
  EndIf
EndProcedure
