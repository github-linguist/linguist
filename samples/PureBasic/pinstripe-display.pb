#White = $FFFFFF ;color

;Create a Pinstripe image
Procedure PinstripeDisplay(width, height)
  Protected x, imgID, psHeight = height / 4, psWidth = 1, psTop, horzBand
  imgID = CreateImage(#PB_Any, width, height)
  If imgID
    StartDrawing(ImageOutput(imgID))
      Repeat
        x = 0
        Repeat
          Box(x, psTop, psWidth, psHeight, #White)
          x + 2 * psWidth
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
  PicID = PinstripeDisplay(WindowWidth(0), WindowHeight(0))
  ImageGadget(0, 0, 0, WindowWidth(0), WindowHeight(0), ImageID(PicID))
  While WaitWindowEvent() <> #PB_Event_CloseWindow
  Wend
EndIf
