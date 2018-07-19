#NoEnv
SetBatchLines, -1
#SingleInstance, Force

; Uncomment if Gdip.ahk is not in your standard library
#Include, Gdip.ahk

; Settings
X := 200, Y := 200, Width := 200, Height := 200 ; Location and size of sphere
rotation := -30 ; degrees
ARGB := 0xFFFF0000 ; Color=Solid Red

If !pToken := Gdip_Startup() ; Start gdi+
{
	MsgBox, 48, gdiplus error!, Gdiplus failed to start. Please ensure you have gdiplus on your system
	ExitApp
}
OnExit, Exit

Gui, -Caption +E0x80000 +LastFound +AlwaysOnTop +ToolWindow +OwnDialogs ; Create GUI
Gui, Show, NA ; Show GUI
hwnd1 := WinExist() ; Get a handle to this window we have created in order to update it later
hbm := CreateDIBSection(A_ScreenWidth, A_ScreenHeight) ; Create a gdi bitmap drawing area
hdc := CreateCompatibleDC() ; Get a device context compatible with the screen
obm := SelectObject(hdc, hbm) ; Select the bitmap into the device context
pGraphics := Gdip_GraphicsFromHDC(hdc) ; Get a pointer to the graphics of the bitmap, for use with drawing functions
Gdip_SetSmoothingMode(pGraphics, 4) ; Set the smoothing mode to antialias = 4 to make shapes appear smother

Gdip_TranslateWorldTransform(pGraphics, X, Y)
Gdip_RotateWorldTransform(pGraphics, rotation)

; Base ellipse
pBrush := Gdip_CreateLineBrushFromRect(0, 0, Width, Height, ARGB, 0xFF000000)
Gdip_FillEllipse(pGraphics, pBrush, 0, 0, Width, Height)

; First highlight ellipse
pBrush := Gdip_CreateLineBrushFromRect(Width*0.1, Height*0.01, Width*0.8, Height*0.6, 0x33FFFFFF, 0x00FFFFFF)
Gdip_FillEllipse(pGraphics, pBrush, Width*0.1, Height*0.01, Width*0.8, Height*0.6)

; Second highlight ellipse
pBrush := Gdip_CreateLineBrushFromRect(Width*0.3, Height*0.02, Width*0.3, Height*0.2, 0xBBFFFFFF, 0x00FFFFFF)
Gdip_FillEllipse(pGraphics, pBrush, Width*0.3, Height*0.02, Width*0.3, Height*0.2)


UpdateLayeredWindow(hwnd1, hdc, 0, 0, A_ScreenWidth, A_ScreenHeight)
SelectObject(hdc, obm) ; Select the object back into the hdc
Gdip_DeletePath(Path)
Gdip_DeleteBrush(pBrush)
DeleteObject(hbm) ; Now the bitmap may be deleted
DeleteDC(hdc) ; Also the device context related to the bitmap may be deleted
Gdip_DeleteGraphics(G) ; The graphics may now be deleted
Return

Exit:
; gdi+ may now be shutdown on exiting the program
Gdip_Shutdown(pToken)
ExitApp
