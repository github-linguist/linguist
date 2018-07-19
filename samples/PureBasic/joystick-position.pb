If InitJoystick() = 0
  MessageRequester("Error!", "Need to connect a joystick", #PB_MessageRequester_Ok)
  End
EndIf

;some constants for Window positioning
#WindowW = 100: #WindowH = 100
#CrossW = 10
#p1 = (#WindowW - #CrossW) / 2
#p2 = (#WindowW / 2 - #CrossW)

If OpenWindow(0, 0, 0, #WindowW * 2 + 10, #WindowH, "Joystick Position", #PB_Window_SystemMenu)
  CreateImage(0, #WindowW, #WindowW)
  ImageGadget(0, 0, 0, 0, 0, ImageID(0))
  TextGadget(2, #WindowW + 5, 10, #WindowW, 20, "Buttons Pressed:")
  CreateImage(1, #WindowW, 40)
  ImageGadget(1,  #WindowW + 5, 30, 0, 0, ImageID(1))

  AddKeyboardShortcut(0, #PB_Shortcut_Escape, 0)
  Define event, x_movement, y_movement
  Repeat
    Repeat
      event = WindowEvent()
      Select event
        Case #PB_Event_Menu
          If EventMenu() = 0
            End
          EndIf
        Case #PB_Event_CloseWindow
          End
      EndSelect
    Until event = 0

    Define pressed.s, buttonNum, buttonX, buttonY, buttonText.s, buttonColor
    pressed.s = ""
    If ExamineJoystick(0)
      x_movement = JoystickAxisX(0)
      y_movement = JoystickAxisY(0)

      StartDrawing(ImageOutput(1))
        DrawingMode(#PB_2DDrawing_Transparent)
        Box(0, 0, #WindowW, 50, RGB($D4, $D0, $C8)) ;a Gray
        ; check to see if any of the buttons have been pressed
        For buttonNum = 1 To 10
          buttonX = ((buttonNum - 1) * 20 + 10) % #WindowW
          buttonY = ((buttonNum - 1) / 5) * 20 + 10
          If JoystickButton(0, buttonNum)
            buttonColor = RGB($FF, 0, 0) ;Red
          Else
            buttonColor = RGB($80, $80, $80) ;Gray
          EndIf
          Circle(buttonX, buttonY, 9, buttonColor)
          buttonText = Str(buttonNum)
          DrawText(buttonX - TextWidth(buttonText) / 2, buttonY - TextHeight(buttonText) / 2, buttonText, RGB($FF, $FF, $FF)) ;White
        Next
      StopDrawing()

      SetGadgetState(1, ImageID(1))
    EndIf


    StartDrawing(ImageOutput(0))
      Box(0,0, #WindowW, #WindowW, RGB($FF, $FF, $FF)) ;White
      Line(#p1 + x_movement * #p2, #WindowW / 2 + y_movement * #p2, #CrossW, 1, RGB($FF, 0, 0)) ;Red
      Line(#WindowW / 2 + x_movement * #p2, #p1 + y_movement * #p2, 1, #CrossW, RGB($FF, 0, 0)) ;Red
    StopDrawing()

    SetGadgetState(0, ImageID(0))

    Delay(10)
  Until event = #PB_Event_CloseWindow
EndIf
