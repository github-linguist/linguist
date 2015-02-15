#SingleInstance, Force
#NoEnv
SetBatchLines, -1
; Uncomment if Gdip.ahk is not in your standard library
;#Include, Gdip.ahk
If !pToken := Gdip_Startup(){
   MsgBox, 48, Gdiplus error!, Gdiplus failed to start. Please ensure you have Gdiplus on your system.
   ExitApp
}
; I've added a simple new function here, just to ensure if anyone is having any problems then to make sure they are using the correct library version
if (Gdip_LibraryVersion() < 1.30)
{
	MsgBox, 48, Version error!, Please download the latest version of the gdi+ library
	ExitApp
}
OnExit, Exit
tour := "a1 b3 d2 c4 a5 b7 d8 e6 d4 b5 c7 a8 b6 c8 a7 c6 b8 a6 b4 d5 e3 d1 b2 a4 c5 d7 f8 h7 f6 g8 h6 f7 h8 g6 e7 f5 h4 g2 e1 d3 e5 g4 f2 h1 g3 f1 h2 f3 g1 h3 g5 e4 d6 e8 g7 h5 f4 e2 c1 a2 c3 b1 a3 c2 "
; Knight's tour with maximum symmetry by George Jelliss, http://www.mayhematics.com/t/8f.htm
; I know, I know, but I followed the task outline to the letter! Besides, this path is the prettiest.

; Input: starting square
InputBox, start, Knight's Tour Start, Enter Knight's starting location in algebraic notation:, , , , , , , , b3
i := InStr(tour, start)
If i=0
{
	Msgbox Error, please try again.
	Reload
}
; Output: move sequence
Msgbox % tour := SubStr(tour, i) . SubStr(tour, 1, i-1)

; Animation
tour .= SubStr(tour, 1, 3)
, CellSize := 30 ; pixels
, Width := Height := 9*CellSize
, TopLeftX := (A_ScreenWidth - Width) // 2
, TopLeftY := (A_ScreenHeight - Height) // 2
Gui, -Caption +E0x80000 +LastFound +AlwaysOnTop +ToolWindow +OwnDialogs
Gui, Show, NA ; show board (currently transparent)
hwnd1 := WinExist() ; required for Gdip
OnMessage(0x201, "WM_LBUTTONDOWN")
, hbm := CreateDIBSection(Width, Height)
, hdc := CreateCompatibleDC()
, obm := SelectObject(hdc, hbm)
, G := Gdip_GraphicsFromHDC(hdc)
, Gdip_SetSmoothingMode(G, 4)

Loop 1 ; remove '1' and uncomment next line to loop infinitely
{
;Gdip_GraphicsClear(G) ; uncomment to loop infinitely
cOdd := "0xFFFFCE9E" ; create brushes
, cEven := "0xFFD18B47"
, pBrushOdd := Gdip_BrushCreateSolid(cOdd)
, pBrushEven := Gdip_BrushCreateSolid(cEven)

Loop 64 ; layout board
{
	Row := mod(A_Index-1,8)+1
	, Col := (A_Index-1)//8+1
	, Gdip_FillRectangle(G, mod(Row+Col,2) ? pBrushOdd : pBrushEven, Col * CellSize + 1, Row * CellSize + 1, CellSize - 2, CellSize - 2)
}
Gdip_DeleteBrush(pBrushOdd) ; cleanup memory
, Gdip_DeleteBrush(pBrushEven)
, UpdateLayeredWindow(hwnd1, hdc, TopLeftX, TopLeftY, Width, Height) ; update board

, pPen := Gdip_CreatePen(0x66FF0000, CellSize/10) ; create pen
, Algebraic := SubStr(tour,1,2) ; get starting coordinates
, x := (Asc(SubStr(Algebraic, 1, 1))-96+0.5)*CellSize
, y := (9.5-SubStr(Algebraic, 2, 1))*CellSize

Loop 64 ; trace path
{
	Sleep, 0.5*1000
	xold := x, yold := y ; a line has start and end points
	, Algebraic := SubStr(tour,(A_Index)*3+1,2) ; get new coordinates
	, x := (Asc(SubStr(Algebraic, 1, 1))-96+0.5)*CellSize
	, y := (9.5-SubStr(Algebraic, 2, 1))*CellSize
	, Gdip_DrawLine(G, pPen, xold, yold, x, y)
	, UpdateLayeredWindow(hwnd1, hdc, TopLeftX, TopLeftY, Width, Height) ; update board
}
Gdip_DeletePen(pPen)
}
Return

GuiEscape:
	ExitApp

Exit:
	Gdip_Shutdown(pToken)
	ExitApp

WM_LBUTTONDOWN()
{
	If (A_Gui = 1)
	PostMessage, 0xA1, 2
}
