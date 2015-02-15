; Uncomment if Gdip.ahk is not in your standard library
; #Include, Gdip.ahk

; Comment for lower CPU usage
SetBatchLines, -1

JoystickNumber := 0     ; (1-16) or (0 = Auto-detect)
CrosshairSize := 100
BarWidth := 50
BarSpacing := BarWidth + 8
Color1 := 0x99000000
Color2 := 0x99ffffff
Color3 := 0x99ff6600
Color4 := 0xff0066ff
Color5 := 0xffff6600
Font := "Arial"
FontSize1 := 20
FontSize2 := 30
Lineweight1 := 8
Lineweight2 := 3
Lineweight3 := 2
Lineweight4 := 4
Show2ndCrosshair := true
AxisLabelHeight := 47

#SingleInstance, Force
#NoEnv
OnExit, Exit
SysGet, MWA, MonitorWorkArea
CrosshairOffset := CrosshairSize // 2
, CircleOffset := CrosshairOffset - 5
, CircleSize := CrosshairSize - 10
, TaskBarHeight := A_ScreenHeight - MWABottom + Lineweight1 // 2
, ScaleX := A_ScreenWidth / 100
, ScaleY1 := (A_ScreenHeight - TaskBarHeight - AxisLabelHeight) / 100
, ScaleY2 := A_ScreenHeight / 100
, BarCenter := (MWABottom - AxisLabelHeight) // 2 + AxisLabelHeight
, BorderBot := MWABottom - Lineweight1 // 2 + 2
, PieSize := 400
, PieX := (A_ScreenWidth - PieSize) // 2
, PieY := (A_ScreenHeight - PieSize) // 2
, BarHeight := A_ScreenHeight - AxisLabelHeight - TaskBarHeight
, AxisTextOffset := BarWidth > 32 ? (BarWidth - 32) // 2 : 0
, Axis_Array := {"X": "X", "Y": "Y"}
, MaxI :=  2

; Auto-detect the joystick number if called for
if (JoystickNumber < 1) {
    Loop, 16 {
        GetKeyState, Joy_Name, %A_Index%JoyName
        if (Joy_Name) {
            JoystickNumber := A_Index
            break
        }
    }
    if (!JoystickNumber) {
        MsgBox The system does not appear to have any joysticks.
        ExitApp
    }
}
else {
    GetKeyState, Joy_Name, %JoystickNumber%JoyName
    if (!Joy_Name) {
        MsgBox The system does not appear to have a joystick number %JoystickNumber%.
        ExitApp
    }
}

if (!pToken := Gdip_Startup()) {
    MsgBox, 48, Gdiplus error!, Gdiplus failed to start. Please ensure you have Gdiplus on your system.
    ExitApp
}

If (!Gdip_FontFamilyCreate(Font)) {
   MsgBox, 48, Font error!, The font you have specified does not exist on your system.
   ExitApp
}

; Get joystick information
SetFormat, FloatFast, 03.2
GetKeyState, Joy_Buttons, % JoystickNumber "JoyButtons"
GetKeyState, Joy_Info, % JoystickNumber "JoyInfo"
Loop, Parse, Joy_Info
    if (A_LoopField != "C" && A_LoopField != "D" && A_LoopField != "P")
        Axis_Array[A_LoopField] := A_LoopField
        , %A_LoopField% := true
        , MaxI++
    else
        %A_LoopField% := true

; Setup Gdip
Gui, 1: -Caption +E0x80000 +LastFound +AlwaysOnTop +ToolWindow +OwnDialogs
Gui, 1: Show, NA
hwnd1 := WinExist()
, hbm := CreateDIBSection(A_ScreenWidth, A_ScreenHeight)
, hdc := CreateCompatibleDC()
, obm := SelectObject(hdc, hbm)
, G1 := Gdip_GraphicsFromHDC(hdc)
, Gdip_SetSmoothingMode(G1, 4)
, pPen1 := Gdip_CreatePen(Color1, Lineweight1)
, pPen2 := Gdip_CreatePen(Color2, Lineweight2)
, pPen3 := Gdip_CreatePen(Color4, Lineweight3)
, pPen4 := Gdip_CreatePen(Color5, Lineweight4)
, pBrush1 := Gdip_BrushCreateSolid(Color1)
, pBrush2 := Gdip_BrushCreateSolid(Color3)

; Crosshair 2
if ((R || U) && Show2ndCrosshair) {
    pPen5 := Gdip_CreatePen(Color5, Lineweight3)
    , pPen6 := Gdip_CreatePen(Color4, Lineweight4)
    , joy_r := joy_u := 50
}

; Bar X-offsets
for key, val in Axis_Array
    %val%X := A_ScreenWidth - MaxI * BarSpacing + BarSpacing * (A_Index - 1) + 3

; Info box
IBH1 := 150
, IBW1 := 450
, IBX1 := A_ScreenWidth - MaxI * BarSpacing - IBW1
, IBY1 := A_ScreenHeight - TaskBarHeight - IBH1 + Lineweight1 // 2
, IBH2 := IBH1 - 8
, IBW2 := IBW1 - 8
, IBX2 := IBX1 + 4
, IBY2 := IBY1 + 4
, FontOptions1 := "x" (IBX1 + 8) " y" (IBY1 + 8) " w" IBW1 - 20 " Left c" SubStr(Color2, 3) " r4 s" FontSize1 " Bold"

; Axis box
ABH1 := AxisLabelHeight + 4
, ABW1 := MaxI * BarSpacing
, ABX1 := A_ScreenWidth - MaxI * BarSpacing
, ABY1 := 0
, ABH2 := ABH1 - 16
, ABW2 := ABW1 - 8
, ABX2 := ABX1 + 4
, ABY2 := ABY1 + 4
, FontOptions2 := " y" ABY1 + AxisLabelHeight - 40 " w" ABW1 - 10 " Left c" SubStr(Color2, 3) " r4 s" FontSize2 " Bold"

; Update graphics
Loop, {
    Buttons_Down := ""
    Loop, %Joy_Buttons% {
        GetKeyState, joy%A_Index%, %JoystickNumber%joy%A_Index%
        if (joy%A_Index% = "D")
            Buttons_Down .= " " A_Index
    }

    ; Info & axis boxes
    InfoText := Joy_Name " (#" JoystickNumber "):`n" Axis_Info "`nButtons Down: " Buttons_Down "`n`n(Ctrl+Esc to exit)"
    , Gdip_FillRoundedRectangle(G1, pBrush1, IBX1, IBY1, IBW1, IBH1, 5)
    , Gdip_DrawRoundedRectangle(G1, pPen2, IBX2, IBY2, IBW2, IBH2, 5)
    , Gdip_TextToGraphics(G1, InfoText, FontOptions1, Font, A_ScreenWidth, A_ScreenHeight)
    , Gdip_FillRoundedRectangle(G1, pBrush1, ABX1, ABY1, ABW1, ABH1, 5)
    , Gdip_DrawRoundedRectangle(G1, pPen2, ABX2, ABY2, ABW2, ABH2, 5)

    ; Axis bars
    Axis_Info := ""
    for key, val in Axis_Array {
        GetKeyState, joy_%val%, % JoystickNumber "Joy" val
        Axis_Info .= val joy_%val% "  "
        if (joy_%val% > 50)
            %val%Y := BarCenter
            , %val%h1 := (joy_%val% - 50) * ScaleY1
        else
            %val%Y := AxisLabelHeight + joy_%val% * ScaleY1  ;
            , Sc - (joy_%val% - 50) * ScaleY1
            , %val%h1 := BarCenter - %val%Y
        Gdip_FillRoundedRectangle(G1, pBrush2, %val%X, %val%Y, BarWidth, %val%h1, 2)
        , Gdip_DrawRoundedRectangle(G1, pPen1, %val%X, AxisLabelHeight, BarWidth, BarHeight, 5)
        , Gdip_DrawRoundedRectangle(G1, pPen2, %val%X, AxisLabelHeight, BarWidth, BarHeight, 5)
        , Gdip_TextToGraphics(G1, val, "x" (%val%X + AxisTextOffset) FontOptions2, Font, A_ScreenWidth, A_ScreenHeight)
    }

    ; POV hat
    If (P) {
        GetKeyState, Joy_P, %JoystickNumber%JoyPOV
        Axis_Info .= "  POV" Joy_P
        if (Joy_P > -1) {
			StartAngle := (Joy_P > 33750 || Joy_P <= 2250) ? 247.5 	; up
			: Joy_P > 29250 ? 202.5		; up left
			: Joy_P > 24750 ? 157.5 	; left
			: Joy_P > 20250 ? 112.5 	; down left
			: Joy_P > 15750 ? 67.5 		; down
			: Joy_P > 11250 ? 22.5 		; down right
			: Joy_P > 6750 ? 337.5		; right
			: 292.5 					; up right
            , Gdip_FillPie(G1, pBrush2, PieX, PieY, PieSize, PieSize, StartAngle, 45)
            , Gdip_DrawPie(G1, pPen1, PieX, PieY, PieSize, PieSize, StartAngle, 45)
            , Gdip_DrawPie(G1, pPen2, PieX, PieY, PieSize, PieSize, StartAngle, 45)
        }
    }

    ; Crosshair 1
    CenterX := ScaleX * joy_x
    , CenterY := ScaleY2 * joy_y
    , Gdip_DrawLine(G1, pPen3, CenterX-CrosshairOffset, CenterY, CenterX+CrosshairOffset, CenterY)
    , Gdip_DrawLine(G1, pPen3, CenterX, CenterY-CrosshairOffset, CenterX, CenterY+CrosshairOffset)
    , Gdip_DrawEllipse(G1, pPen4, CenterX-CircleOffset, CenterY-CircleOffset, CircleSize, CircleSize)
    , Gdip_DrawEllipse(G1, pPen4, CenterX-3, CenterY-3, 6, 6)

    ; Crosshair 2
    if ((R || U) && Show2ndCrosshair)
        CenterU := ScaleX * joy_u
        , CenterR := ScaleY2 * joy_r
        , Gdip_DrawLine(G1, pPen5, CenterU-CrosshairOffset, CenterR, CenterU+CrosshairOffset, CenterR)
        , Gdip_DrawLine(G1, pPen5, CenterU, CenterR-CrosshairOffset, CenterU, CenterR+CrosshairOffset)
        , Gdip_DrawEllipse(G1, pPen6, CenterU-CircleOffset, CenterR-CircleOffset, CircleSize, CircleSize)
        , Gdip_DrawEllipse(G1, pPen6, CenterU-3, CenterR-3, 6, 6)

    UpdateLayeredWindow(hwnd1, hdc, 0, 0, A_ScreenWidth, A_ScreenHeight)
    , Gdip_GraphicsClear(G1)
}
return

^Esc::
Exit:
Gdip_Shutdown(pToken)
ExitApp
