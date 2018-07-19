If Not InitKeyboard(): End: EndIf    ;can't init keyboard
If Not InitSprite(): End: EndIf      ;can't init sprite/screen library
If Not ExamineDesktops(): End: EndIf ;can't retrieve information about desktop

Define height.f, width.f, depth
height.f = DesktopHeight(0)
width.f = DesktopWidth(0)
depth = DesktopDepth(0)

If OpenScreen(width, height, depth, "Press ENTER to exit")
  Define vsCount, v, h, columns, columnWidth, endColor, shade
  StartDrawing(ScreenOutput())
    vsCount = 4
    For v = 0 To 3
      columns = (v + 1) * 8
      columnWidth = Round(width / columns, #PB_Round_Up)
      endColor = $FFFFFF * (v % 2)     ;alternate between black and white for first and last bar
      Box(0, (height * v) / vsCount, columnWidth, height / vsCount, endColor)

      For h = 1 To columns - 2
        If v % 2 = 0
          shade = 256 / columns * (h + 1)
        Else
          shade = 256 / columns * (columns - (h + 1))
        EndIf
        Box((width * h) / columns, (height * v) / vsCount, columnWidth, height / vsCount, RGB(shade, shade, shade))
      Next

      Box((width * (columns - 1)) / columns, (height * v) / vsCount, columnWidth, height / vsCount, $FFFFFF - endColor)
    Next
  StopDrawing()
  FlipBuffers()

  Repeat
    Delay(10)
    ExamineKeyboard()
  Until KeyboardPushed(#PB_Key_Escape) Or KeyboardPushed(#PB_Key_Return)
  CloseScreen()
EndIf
