Procedure getHistogram(image, Array histogram(1))
  Protected w = ImageWidth(image) - 1
  Protected h = ImageHeight(image) - 1
  Dim histogram(255) ;output

  StartDrawing(ImageOutput(image))
    For x = 0 To w
      For y = 0 To h
        lum = Red(Point(x, y)) ;the Green or Blue color components could be used also
        histogram(lum) + 1
      Next
    Next
  StopDrawing()
EndProcedure

Procedure median(Array histogram(1))
  Protected low, high = 255, left, right

  While low <> high
    If left < right
      low + 1
      left + histogram(low)
    Else
      high - 1
      right + histogram(high)
    EndIf
  Wend
  ProcedureReturn low
EndProcedure

Procedure blackAndWhite(image, median)
  Protected w = ImageWidth(image) - 1
  Protected h = ImageHeight(image) - 1
  CallDebugger
  StartDrawing(ImageOutput(image))
    For x = 0 To w
      For y = 0 To h
        If Red(Point(x, y)) < median ;the Green or Blue color components could be used also
          Plot(x, y, $000000) ;black
        Else
          Plot(x, y, $FFFFFF) ;white
        EndIf
      Next
    Next
  StopDrawing()
EndProcedure

Define sourceFile.s, outputFile.s, image = 3, m
Dim histogram(255)

sourceFile = OpenFileRequester("Select source image file", "*.ppm", "PPM image (*.ppm)|PPM", 0)

If sourceFile And LCase(GetExtensionPart(sourceFile)) = "ppm"
  LoadImagePPM(image, sourceFile)
  ImageGrayout(image)

  getHistogram(image,histogram())
  m = median(histogram())
  blackAndWhite(image, m)

  outputFile = Left(sourceFile, Len(sourceFile) - Len(GetExtensionPart(sourceFile))) + "_bw." + GetExtensionPart(sourceFile)
  SaveImageAsPPM(image, outputFile, 1)
EndIf
