;Create a Pinstripe image with a pattern of vertical stripe colors
Procedure PinstripeDisplay(width, height, Array psColors(1), numColors = 0)
  Protected x, imgID, psHeight = height / 4, psWidth = 1, psTop, horzBand, curColor

  If numColors < 1: numColors = ArraySize(psColors()) + 1: EndIf

  imgID = CreateImage(#PB_Any, width, height)
  If imgID
    StartDrawing(ImageOutput(imgID))
      Repeat
        x = 0
        curColor = 0
        Repeat
          Box(x, psTop, psWidth, psHeight, psColors(curColor))
          curColor = (curColor + 1) % numColors
          x + psWidth
        Until x >= width
        psWidth + 1
        horzBand + 1
        psTop = horzBand * height / 4  ;move to the top of next horizontal band of image
      Until psTop >= height
    StopDrawing()
  EndIf
  ProcedureReturn imgID
EndProcedure

;Open a window and display the pinstripe
If OpenWindow(0, 0, 0, 1, 1,"PureBasic Pinstripe", #PB_Window_Maximize | #PB_Window_SystemMenu)
  Dim psColors(7)
  psColors(0) = RGB($00, $00, $00) ;black
  psColors(1) = RGB($FF, $00, $00) ;red
  psColors(2) = RGB($00, $FF, $00) ;green
  psColors(3) = RGB($00, $00, $FF) ;blue
  psColors(4) = RGB($FF, $00, $FF) ;magenta
  psColors(5) = RGB($00, $FF, $FF) ;cyan
  psColors(6) = RGB($FF, $FF, $00) ;yellow
  psColors(7) = RGB($FF, $FF, $FF) ;white

  PicID = PinstripeDisplay(WindowWidth(0), WindowHeight(0), psColors())
  ImageGadget(0, 0, 0, WindowWidth(0), WindowHeight(0), ImageID(PicID))
  While WaitWindowEvent() <> #PB_Event_CloseWindow
  Wend
EndIf
