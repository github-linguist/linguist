#NoEnv
#SingleInstance, Force
SetBatchLines, -1

; Uncomment if Gdip.ahk is not in your standard library
;#Include, Gdip.ahk

; Start gdi+
If !pToken := Gdip_Startup()
{
	MsgBox, 48, gdiplus error!, Gdiplus failed to start. Please ensure you have gdiplus on your system
	ExitApp
}
OnExit, Exit
; I've added a simple new function here, just to ensure if anyone is having any problems then to make sure they are using the correct library version
If (Gdip_LibraryVersion() < 1.30)
{
	MsgBox, 48, version error!, Please download the latest version of the gdi+ library
	ExitApp
}
x1:=300,y1:=500,r1:=50,x2:=200,y2:=200,r2:=150,x3:=600,y3:=400,r3:=100,s1:=-1,s2:=-1,s3:=-1,xs:=0,ys:=0,rs:=0
, Apollonius(x1,y1,r1,x2,y2,r2,x3,y3,r3,s1,s2,s3,xs,ys,rs)
, Width:=max(x1+r1 "," x2+r2 "," x3+r3 "," xs+rs)*1.1
, Height:=max(y1+r1 "," y2+r2 "," y3+r3 "," ys+rs)*1.1

Gui, -Caption +E0x80000 +LastFound +AlwaysOnTop +ToolWindow +OwnDialogs
Gui, Show
hwnd1 := WinExist()
, hbm := CreateDIBSection(Width, Height)
, hdc := CreateCompatibleDC()
, obm := SelectObject(hdc, hbm)
, G := Gdip_GraphicsFromHDC(hdc)
, Gdip_SetSmoothingMode(G, 4)
, bWhite := Gdip_BrushCreateSolid(0xffffffff)
, Gdip_FillRectangle(G, bWhite, 0, 0, Width, Height)
, pRed := Gdip_CreatePen(0x88ff0000, 3)
, pGreen := Gdip_CreatePen(0x8800ff00, 3)
, pBlue := Gdip_CreatePen(0x880000ff, 3)
, pBlack := Gdip_CreatePen(0x88000000, 3)
, Gdip_DrawCircle(G, pRed, x1, y1, r1)
, Gdip_DrawCircle(G, pGreen, x2, y2, r2)
, Gdip_DrawCircle(G, pBlue, x3, y3, r3)
, Gdip_DrawCircle(G, pBlack, xs, ys, rs)
, Gdip_DeletePen(pRed)
, Gdip_DeletePen(pGreen)
, Gdip_DeletePen(pBlue)
, Gdip_DeletePen(pBlack)
, UpdateLayeredWindow(hwnd1, hdc, 0, 0, Width, Height)
Return

GuiEscape:
GuiClose:
Exit:
	SelectObject(hdc, obm)
	, DeleteObject(hbm)
	, DeleteDC(hdc)
	, Gdip_DeleteGraphics(G)
	, Gdip_Shutdown(pToken)
	ExitApp

Apollonius(x1=300,y1=500,r1=50,x2=200,y2=200,r2=150,x3=600,y3=400,r3=100,s1=1,s2=1,s3=1,ByRef xs=0, ByRef ys=0, ByRef rs=0) {
v11 := 2*x2 - 2*x1
v12 := 2*y2 - 2*y1
v13 := x1**2 - x2**2 + y1**2 - y2**2 - r1**2 + r2**2
v14 := 2*s2*r2 - 2*s1*r1

v21 := 2*x3 - 2*x2
v22 := 2*y3 - 2*y2
v23 := x2**2 - x3**2 + y2**2 - y3**2 - r2**2 + r3**2
v24 := 2*s3*r3 - 2*s2*r2

w12 := v12/v11
w13 := v13/v11
w14 := v14/v11

w22 := v22/v21 - w12
w23 := v23/v21 - w13
w24 := v24/v21 - w14

p := -w23/w22
q := w24/w22
m := -w12*p - w13
n := w14 - w12*q

a := n**2 + q**2 - 1
b := 2*m*n - 2*n*x1 + 2*p*q - 2*q*y1 + 2*s1*r1
c := x1**2 + m**2 - 2*m*x1 + p**2 + y1**2 - 2*p*y1 - r1**2

d := b**2 - 4*a*c
rs := (-b - d**0.5)/(2*a)
xs := m + n*rs
ys := p + q*rs
}

; from http://rosettacode.org/wiki/Greatest_element_of_a_list#AutoHotkey
max(list) {
	Loop Parse, list, `,
	x := x < A_LoopField ? A_LoopField : x
	Return x
}

; Gdip helper function
Gdip_DrawCircle(G, pPen, x, y, r) {
	Return Gdip_DrawEllipse(G, pPen, x-r, y-r, r*2, r*2)
}
