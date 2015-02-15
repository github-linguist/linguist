#SingleInstance, Force
#NoEnv
SetBatchLines, -1
OnExit, Exit
FileOut := A_Desktop "\MyNewFile.png"
Font := "Arial"
x := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
y := [2.7, 2.8, 31.4, 38.1, 58.0, 76.2, 100.5, 130.0, 149.3, 180.0]
; Uncomment if Gdip.ahk is not in your standard library
; #Include, Gdip.ahk
if (!pToken := Gdip_Startup()) {
	MsgBox, 48, Gdiplus error!, Gdiplus failed to start. Please ensure you have Gdiplus on your system.
	ExitApp
}
If (!Gdip_FontFamilyCreate(Font)) {
   MsgBox, 48, Font error!, The font you have specified does not exist on your system.
   ExitApp
}

pBitmap := Gdip_CreateBitmap(900, 900)
, G := Gdip_GraphicsFromImage(pBitmap)
, Gdip_SetSmoothingMode(G, 4)
, pBrush := Gdip_BrushCreateSolid(0xff000000)
, Gdip_FillRectangle(G, pBrush, -3, -3, 906, 906)
, Gdip_DeleteBrush(pBrush)
, pPen1 := Gdip_CreatePen(0xffffcc00, 2)
, pPen2 := Gdip_CreatePen(0xffffffff, 2)
, pPen3 := Gdip_CreatePen(0xff447821, 1)
, pPen4 := Gdip_CreatePen(0xff0066ff, 2)
, Gdip_DrawLine(G, pPen2, 50, 50, 50, 850)
, Gdip_DrawLine(G, pPen2, 50, 850, 850, 850)
, FontOptions1 := "x0 y870 Right cbbffffff r4 s16 Bold"
, Gdip_TextToGraphics(G, 0, FontOptions1, Font, 40, 20)

Loop, % x.MaxIndex() - 1 {
	Offset1 := 50 + (x[A_Index] * 80)
	, Offset2 := Offset1 + 80
	, Gdip_DrawLine(G, pPen1, Offset1, 850 - (y[A_Index] * 4), Offset1 + 80, 850 - (y[A_Index + 1] * 4))
}

Loop, % x.MaxIndex() {
	Offset1 := 50 + ((A_Index - 1) * 80)
	, Offset2 := Offset1 + 80
	, Offset3 := 45 + (x[A_Index] * 80)
	, Offset4 := 845 - (y[A_Index] * 4)
	, Gdip_DrawLine(G, pPen2, 45, Offset1, 55, Offset1)
	, Gdip_DrawLine(G, pPen2, Offset2, 845, Offset2, 855)
	, Gdip_DrawLine(G, pPen3, 50, Offset1, 850, Offset1)
	, Gdip_DrawLine(G, pPen3, Offset2, 50, Offset2, 850)
	, Gdip_DrawLine(G, pPen4, Offset3, Offset4, Offset3 + 10, Offset4 + 10)
	, Gdip_DrawLine(G, pPen4, Offset3, Offset4 + 10, Offset3 + 10, Offset4)
	, FontOptions1 := "x0 y" (Offset1 - 7) " Right cbbffffff r4 s16 Bold"
	, FontOptions2 := "x" (Offset2 - 7) " y870 Left cbbffffff r4 s16 Bold"
	, Gdip_TextToGraphics(G, 220 - (A_Index * 20), FontOptions1, Font, 40, 20)
	, Gdip_TextToGraphics(G, A_Index, FontOptions2, Font, 40, 20)
}

Gdip_DeletePen(pPen1)
, Gdip_DeletePen(pPen2)
, Gdip_DeletePen(pPen3)
, Gdip_DeletePen(pPen4)
, Gdip_SaveBitmapToFile(pBitmap, FileOut)
, Gdip_DisposeImage(pBitmap)
, Gdip_DeleteGraphics(G)
Run, % FileOut

Exit:
Gdip_Shutdown(pToken)
ExitApp
