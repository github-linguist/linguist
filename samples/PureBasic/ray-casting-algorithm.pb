Structure point_f
  x.f
  y.f
EndStructure
Procedure inpoly(*p.point_f, List poly.point_f())
  Protected.point_f new, old, lp, rp
  Protected inside
  If ListSize(poly()) < 3: ProcedureReturn 0: EndIf
  LastElement(poly()): old = poly()
  ForEach poly()
    ;find leftmost endpoint 'lp' and the rightmost endpoint 'rp' based on x value
    If poly()\x > old\x
      lp = old
      rp = poly()
    Else
      lp = poly()
      rp = old
    EndIf
    If lp\x < *p\x And *p\x <= rp\x And (*p\y - lp\y) * (rp\x - lp\x) < (rp\y - lp\y) * (*p\x - lp\x)
      inside = ~inside
    EndIf
    old = poly()
  Next
  ProcedureReturn inside & 1
EndProcedure

If InitSprite()
  If InitKeyboard() And InitMouse()
    OpenWindow(0, 0, 0, 800, 600, "Press [Esc] to close, [Left mouse button] Add Point, [Right mouse button] Clear All Points.", #PB_Window_ScreenCentered | #PB_Window_SystemMenu)
    OpenWindowedScreen(WindowID(0), 0, 0, 800, 600, 1, 0, 0)
    SetFrameRate(60)
  EndIf
Else
  MessageRequester("", "Unable to initsprite"): End
EndIf

NewList v.point_f()
Define.point_f pvp, mp
Define Col, EventID, mode.b, modetxt.s
Repeat
  Delay(1)
  EventID = WindowEvent()
  ExamineKeyboard()
  ExamineMouse()
  ClearScreen(Col)

  mp\x = MouseX()
  mp\y = MouseY()
  If MouseButton(#PB_MouseButton_Left)
    AddElement(v())
    v()\x = mp\x
    v()\y = mp\y
    Delay(100)
  EndIf

  If MouseButton(#PB_MouseButton_Right)
    ClearList(v())
    Delay(100)
  EndIf

  StartDrawing(ScreenOutput())
    If LastElement(v())
      pvp = v()
      ForEach v()
        LineXY(pvp\x, pvp\y, v()\x, v()\y, RGB(0, $FF, 0)) ;Green
        Circle(pvp\x, pvp\y, 5, RGB($FF, 0, 0)) ;Red
        pvp = v()
      Next
    EndIf
    Circle(MouseX(), MouseY(), 5, RGB($C0, $C0, $FF)) ;LightBlue

    If inpoly(mp, v())
      modetxt = "You are in the polygon."
      Col = RGB(0, 0, 0)
    Else
      modetxt = "You are not in the polygon."
      Col = RGB($50, $50, $50)
    EndIf
    DrawText((800 - TextWidth(modetxt)) / 2, 0, modetxt)
  StopDrawing()

  FlipBuffers()
Until KeyboardReleased(#PB_Key_Escape) Or EventID = #PB_Event_CloseWindow
