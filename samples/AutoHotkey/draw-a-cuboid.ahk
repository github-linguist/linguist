Angle := 45
C := 0.01745329252
W := 200
H := 300
L := 400
LX := L * Cos(Angle * C), LY := L * Sin(Angle * C)

If !pToken := Gdip_Startup()
{
   MsgBox, 48, gdiplus error!, Gdiplus failed to start. Please ensure you have gdiplus on your system
   ExitApp
}
OnExit, Exit

A := 50, B := 650, WinWidth := 700, WinHeight := 700
TopX := (A_ScreenWidth - WinWidth) //2, TopY := (A_ScreenHeight - WinHeight) //2

Gui, 1: -Caption +E0x80000 +LastFound +AlwaysOnTop +ToolWindow +OwnDialogs
Gui, 1: Show, NA
hwnd1 := WinExist(), hbm := CreateDIBSection(WinWidth, WinHeight), hdc := CreateCompatibleDC()
	, obm := SelectObject(hdc, hbm), G := Gdip_GraphicsFromHDC(hdc), Gdip_SetSmoothingMode(G, 4)

Points := A "," B "|" A+W "," B "|" A+W "," B-H "|" A "," B-H
	, DrawFace(Points, 0xff0066ff, G)

Points := A+W "," B "|" A+W+LX "," B-LY "|" A+W+LX "," B-LY-H "|" A+W "," B-H
	, DrawFace(Points, 0xff00d400, G)

Points := A "," B-H "|" A+W "," B-H "|" A+W+LX "," B-LY-H "|" A+LX "," B-LY-H
	, DrawFace(Points, 0xffd40055, G)

UpdateLayeredWindow(hwnd1, hdc, TopX, TopY, WinWidth, WinHeight)

SelectObject(hdc, obm), DeleteObject(hbm), DeleteDC(hdc)
	, Gdip_DeleteGraphics(G)
return

DrawFace(Points, Color, G) {
	pBrush := Gdip_BrushCreateSolid(Color)
		, Gdip_FillPolygon(G, pBrush, Points, 1)
		, Gdip_DeleteBrush(pBrush)
	return
}

Esc::
Exit:
Gdip_Shutdown(pToken)
ExitApp
