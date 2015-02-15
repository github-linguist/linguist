Structure hexGadget
  text.s
  Status.i     ;nonselected = 0, selected = 1
  center.POINT ;location of hex's center
  List shape.POINT()
EndStructure

Structure honeycomb
  gadgetID.i
  margins.POINT
  unusedLetters.s
  chosen.s
  maxLength.i
  Array hexGadgets.hexGadget(0)
  textY.i
EndStructure

Prototype hexEvent_prt(*h.honeycomb, hexID)

Procedure inpoly(*p.POINT, List poly.POINT())
  ;returns 1 if point is inside the polygon defined by poly(), otherwise returns 0
  Protected new.POINT, old.POINT, lp.POINT, rp.POINT, i, inside, *poly
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

;draw a hex Gadget by number
Procedure drawhex(*h.honeycomb, hexID)
  With *h\hexGadgets(hexID)
    Protected p.POINT
    If LastElement(\shape())
      p = \shape()
    EndIf
    ForEach \shape()
      LineXY(p\x, p\y, \shape()\x, \shape()\y, RGB(0, 0, 0)) ;black
      p = \shape()
    Next
    DrawingMode(#PB_2DDrawing_Transparent)
    DrawingFont(FontID(0))
    If \Status
      FillArea(\center\x + 1, \center\y + 1, RGB(0, 0, 0), RGB($FF, 0, $FF))    ;magenta
      DrawText(\center\x - TextWidth(\text) / 2, \center\y - TextHeight(\text) / 2, \text, RGB(0, 0, 1)) ;black, almost
    Else
      FillArea(\center\x + 1, \center\y + 1, RGB(0, 0, 0), RGB($FF, $FF, 0)) ;yellow
      DrawText(\center\x - TextWidth(\text) / 2, \center\y - TextHeight(\text) / 2, \text, RGB($FF, 0, 0)) ;red
    EndIf
  EndWith
EndProcedure

Procedure selectHex(*h.honeycomb, hexID)
  If Not *h\hexGadgets(hexID)\Status
    *h\chosen + *h\hexGadgets(hexID)\text
    *h\hexGadgets(hexID)\Status = 1
    StartDrawing(CanvasOutput(*h\gadgetID))
      drawhex(*h, hexID)
      DrawingMode(#PB_2DDrawing_Default)
      DrawingFont(#PB_Default)
      DrawText(0, *h\textY, "Chosen: " + *h\chosen)
      DrawText(0, *h\textY + 20, "The user chose letter " + *h\hexGadgets(hexID)\text + ".  ")
    StopDrawing()
    ProcedureReturn 1
  EndIf
EndProcedure

Procedure hexKey(*h.honeycomb, hexID)
  If UCase(Chr(GetGadgetAttribute(*h\gadgetID, #PB_Canvas_Input))) = *h\hexGadgets(hexID)\text
    ProcedureReturn selectHex(*h, hexID)
  EndIf
EndProcedure

Procedure hexMouse(*h.honeycomb, hexID)
  Protected mPos.POINT
  mPos\x = GetGadgetAttribute(*h\gadgetID, #PB_Canvas_MouseX)
  mPos\y = GetGadgetAttribute(*h\gadgetID, #PB_Canvas_MouseY)
  If inpoly(mPos,*h\hexGadgets(hexID)\shape())
    ProcedureReturn selectHex(*h, hexID)
  EndIf
EndProcedure

Procedure honeycombEvents(*h.honeycomb)
  If Len(*h\chosen) >= *h\maxLength: ProcedureReturn: EndIf

  Protected event = EventType(), *eventFunction.hexEvent_prt
  Select event
    Case #PB_EventType_Input
      *eventFunction = @hexKey()
    Case #PB_EventType_LeftButtonUp
      *eventFunction = @hexMouse()
    Case #PB_EventType_LostFocus
      SetActiveGadget(*h\gadgetID)
  EndSelect

  If *eventFunction
    For hexID = 0 To ArraySize(*h\hexGadgets())
      If *eventFunction(*h, hexID)
        Break ;event successfully handled
      EndIf
    Next
  EndIf
EndProcedure

Procedure createHexGadget(*h.honeycomb, hexID, x, y, dx, dy)
  With *h\hexGadgets(hexID)
    If *h\unusedLetters
      Protected letterNum = Random(Len(*h\unusedLetters) - 1) + 1
      \text = Mid(*h\unusedLetters, letterNum, 1)
      *h\unusedLetters = ReplaceString(*h\unusedLetters, \text, "")
    EndIf
    \center\x = x: \center\y = y
    AddElement(\shape()): \shape()\x = x - dx:     \shape()\y = y
    AddElement(\shape()): \shape()\x = x - dx / 2: \shape()\y = y + dy
    AddElement(\shape()): \shape()\x = x + dx / 2: \shape()\y = y + dy
    AddElement(\shape()): \shape()\x = x + dx:     \shape()\y = y
    AddElement(\shape()): \shape()\x = x + dx / 2: \shape()\y = y - dy
    AddElement(\shape()): \shape()\x = x - dx / 2: \shape()\y = y - dy
  EndWith
EndProcedure

Procedure initHoneycomb(*h.honeycomb, posX, posY, dx = 30, dy = 25, marginX = 10, marginY = 5)
  Protected i, sx, sy, hCols = 5, hRows = 4, hexGadgetCount = hCols * hRows - 1
  If Not *h: ProcedureReturn 0: EndIf

  *h\unusedLetters.s = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  *h\chosen = ""
  *h\maxLength = 20

  Dim *h\hexGadgets(hexGadgetCount)
  ;calculate size width, height and create honeycomb with margins
  sx = Round(dx * (0.5 + hCols * 1.5), #PB_Round_Nearest) + 1 + 2 * marginX
  sy = dy * (2 * hRows + 1) + 1 + 2 * marginY + 2 * 20 ;includes room for hex, margins, and text
  *h\textY = sy - 2 * 20

  ;create hexes
  Protected hexID, column, row, x, y, baseX, baseY, majorOffsetY = dy
  baseX = dx + marginX
  For column = 0 To hCols - 1
    baseY = dy + marginY
    majorOffsetY ! dy
    For row = 0 To hRows - 1
      x = baseX
      y = baseY + majorOffsetY
      createHexGadget(*h, hexID, x, y, dx, dy)
      baseY + dy * 2
      hexID + 1
    Next
    baseX + dx * 1.5
  Next

  ;draw honeycomb
  *h\gadgetID = CanvasGadget(#PB_Any, posX, posY, sx, sy, #PB_Canvas_Keyboard | #PB_Canvas_ClipMouse)
  If *h\gadgetID = 0: ProcedureReturn 0: EndIf ;failed to created honeycomb

  LoadFont(0, "Arial", 24, #PB_Font_Bold)
  StartDrawing(CanvasOutput(*h\gadgetID))
    For i = 0 To ArraySize(*h\hexGadgets())
      drawhex(*h, i)
    Next
    Box(0, *h\textY, sx, 40, RGB(0, 0, 0)) ;draw black text box
  StopDrawing()
  ProcedureReturn 1
EndProcedure

If OpenWindow(0, 0, 0, 400, 400, "PureBasic - Honeycombs", #PB_Window_SystemMenu)
  Define honeycomb.honeycomb, quit
  If Not initHoneycomb(honeycomb, 0, 0): End: EndIf
  ResizeWindow(0, #PB_Ignore, #PB_Ignore, GadgetWidth(honeycomb\gadgetID), GadgetHeight(honeycomb\gadgetID))
  SetActiveGadget(honeycomb\gadgetID)

  Repeat
    event = WaitWindowEvent()

    Select event
      Case #PB_Event_Gadget
        If EventGadget() = honeycomb\gadgetID
          honeycombEvents(honeycomb)
          If Len(honeycomb\chosen) = honeycomb\maxLength
            MessageRequester("Exit", "You chose: " + honeycomb\chosen + ".")
            quit = 1
          EndIf
        EndIf
      Case #PB_Event_CloseWindow
        quit = 1
    EndSelect

  Until quit = 1
  FreeGadget(honeycomb\gadgetID)
  CloseWindow(0)
EndIf
