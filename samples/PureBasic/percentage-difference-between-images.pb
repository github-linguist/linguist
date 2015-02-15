#URL1="http://rosettacode.org/mw/images/3/3c/Lenna50.jpg"
#URL2="http://rosettacode.org/mw/images/b/b6/Lenna100.jpg"

Procedure.s GetTempFileName(basename$="",Extension$=".tmp")
  Protected file$, i
  Repeat: file$=GetTemporaryDirectory()+basename$+"_"+Str(i)+Extension$: i+1
  Until FileSize(file$) = -1 ; E.g. File not found
  ProcedureReturn file$
EndProcedure

Procedure ImageToMatrix(Image,Array P(2))
  Protected Width=ImageWidth(0)-1, Height=ImageHeight(0)-1, x, y
  ; Scaling down Width & Height by -1 to compensate for using 0-based arrays
  Dim P(Width,Height)
  StartDrawing(ImageOutput(Image))
  For x=0 To Width
    For y=0 To Height
      P(x,y)=Point(x,y)
    Next y
  Next x
  StopDrawing()
EndProcedure

Define File1$, File2$, totalDiff, x, y, w, h

; Load the pictures from RoettaCode
InitNetwork()
File1$=GetTempFileName("",".jpg"): ReceiveHTTPFile(#URL1,File1$)
File2$=GetTempFileName("",".jpg"): ReceiveHTTPFile(#URL2,File2$)

; Decode the images & clean up temporary files
UseJPEGImageDecoder()
LoadImage(0,File1$):LoadImage(1,File2$)
DeleteFile(File1$): DeleteFile(File2$)

; Make two 2D arrays to hold the data
Dim Pic1(0,0): Dim Pic2(0,0)

;Load the image data into the matrixes
ImageToMatrix(0,Pic1()): ImageToMatrix(1,Pic2())

; Compare the data
w=ArraySize(pic1()): h=ArraySize(pic1(),2)
For x=0 To w
  For y=0 To h
    totalDiff+ Abs(  Red(Pic1(x,y)) -   Red(Pic2(x,y)))
    totalDiff+ Abs(Green(Pic1(x,y)) - Green(Pic2(x,y)))
    totalDiff+ Abs( Blue(Pic1(x,y)) -  Blue(Pic2(x,y)))
  Next y
Next x

MessageRequester("Result","Diff= "+StrD(100*totalDiff/(255*w*h*3),3)+" %")
