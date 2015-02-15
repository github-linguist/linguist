#NoEnv
#SingleInstance, Force
SetBatchLines, -1

; Parameters
Width := 512, Height := Width/2*3**0.5, n := 8 ; iterations = 8

; Uncomment if Gdip.ahk is not in your standard library
#Include ..\lib\Gdip.ahkl

If !pToken := Gdip_Startup() ; Start gdi+
{
	MsgBox, 48, gdiplus error!, Gdiplus failed to start. Please ensure you have gdiplus on your system
	ExitApp
}
; I've added a simple new function here, just to ensure if anyone is having any problems then to make sure they are using the correct library version
if (Gdip_LibraryVersion() < 1.30)
{
	MsgBox, 48, Version error!, Please download the latest version of the gdi+ library
	ExitApp
}
OnExit, Exit

; Create a layered window (+E0x80000 : must be used for UpdateLayeredWindow to work!) that is always on top (+AlwaysOnTop), has no taskbar entry or caption
Gui, -Caption +E0x80000 +LastFound +OwnDialogs +Owner +AlwaysOnTop
Gui, Show
hwnd1 := WinExist()
OnMessage(0x201, "WM_LBUTTONDOWN")

, hbm := CreateDIBSection(Width, Height)
, hdc := CreateCompatibleDC()
, obm := SelectObject(hdc, hbm)
, G := Gdip_GraphicsFromHDC(hdc)
, Gdip_SetSmoothingMode(G, 4)

; Sierpinski triangle by subtracting triangles
, pBrushBlack := Gdip_BrushCreateSolid(0xff000000)
, rectangle := 0 "," 0 "|" 0 "," Height "|" Width "," Height "|" Width "," 0
, Gdip_FillPolygon(G, pBrushBlack,  rectangle, FillMode=0)

, pBrushBlue := Gdip_BrushCreateSolid(0xff0000ff)
, triangle := Width/2 "," 0 "|" 0 "," Height "|" Width "," Height
, Gdip_FillPolygon(G, pBrushBlue,  triangle, FillMode=0)
, Gdip_DeleteBrush(pBrushBlue)

, UpdateLayeredWindow(hwnd1, hdc, (A_ScreenWidth-Width)/2, (A_ScreenHeight-Height)/2, Width, Height)

, k:=2, x:=0, y:=0, i:=1
Loop, % n
{
	Sleep 0.5*1000
	While x*y<Width*Height
	{
		triangle := x "," y "|" x+Width/2/k "," y+Height/k "|" x+Width/k "," y
		, Gdip_FillPolygon(G, pBrushBlack,  triangle, FillMode=0)
		, x += Width/k
		, (x >= Width) ? (x := i*Width/2/k, y += Height/k, i:=!i) : ""
	}
	UpdateLayeredWindow(hwnd1, hdc, (A_ScreenWidth-Width)/2, (A_ScreenHeight-Height)/2, Width, Height)
	, k*=2, x:=0, y:=0, i:=1
}

Gdip_DeleteBrush(pBrushBlack)

, UpdateLayeredWindow(hwnd1, hdc, (A_ScreenWidth-Width)/2, (A_ScreenHeight-Height)/2, Width, Height)
Sleep, 1*1000

; Bonus: Sierpinski triangle by random dots
Gdip_GraphicsClear(G, 0xff000000)
, pBrushBlue := Gdip_BrushCreateSolid(0xff0000ff)
, x1:=Width/2, y1:=0, x2:=0, y2:=Height, x3:=Width, y3:=Height
, x:= Width/2, y:=Height/2 ; I'm to lazy to pick a random point.
Loop, % n
{
	Loop, % 10*10**(A_Index/2)
	{
		Random, rand, 1, 3
		x := abs(x+x%rand%)/2
		, y := abs(y+y%rand%)/2
		, Gdip_FillEllipse(G, pBrushBlue, x, y, 1, 1)
	}
	UpdateLayeredWindow(hwnd1, hdc, (A_ScreenWidth-Width)/2, (A_ScreenHeight-Height)/2, Width, Height)
	Sleep, 0.5*1000
}
SelectObject(hdc, obm)
, DeleteObject(hbm)
, DeleteDC(hdc)
, Gdip_DeleteGraphics(G)
Return

Exit:
	Gdip_Shutdown(pToken)
	ExitApp

WM_LBUTTONDOWN()
{
	If (A_Gui = 1)
	PostMessage, 0xA1, 2
}
