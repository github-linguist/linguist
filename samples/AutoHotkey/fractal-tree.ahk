#SingleInstance, Force
#NoEnv
SetBatchLines, -1

; Uncomment if Gdip.ahk is not in your standard library
; #Include, Gdip.ahk

FileOut		:= A_Desktop "\MyNewFile.png"
TreeColor	:= 0xff0066ff	; ARGB
TrunkWidth 	:= 10		; Pixels
TrunkLength	:= 80		; Pixels
Angle 		:= 60		; Degrees
ImageWidth 	:= 670		; Pixels
ImageHeight 	:= 450		; Pixels
Branches	:= 13
Decrease	:= 0.81

Angle := (Angle * 0.01745329252) / 2
	, Points := {}
	, Points[1, "Angle"] := 0
	, Points[1, "X"] := ImageWidth // 2
	, Points[1, "Y"] := ImageHeight - TrunkLength

if (!pToken := Gdip_Startup()) {
	MsgBox, 48, Gdiplus error!, Gdiplus failed to start. Please ensure you have Gdiplus on your system.
	ExitApp
}
OnExit, Exit

pBitmap := Gdip_CreateBitmap(ImageWidth, ImageHeight)
	, G := Gdip_GraphicsFromImage(pBitmap)
	, Gdip_SetSmoothingMode(G, 4)
	, pBrush := Gdip_BrushCreateSolid(0xff000000)
	, Gdip_FillRectangle(G, pBrush, -5, -5, ImageWidth + 10, ImageHeight + 10)
	, Gdip_DeleteBrush(pBrush)
	, pPen := Gdip_CreatePen(TreeColor, TrunkWidth/Decrease)
	, Gdip_DrawLine(G, pPen, Points.1.X, Points.1.Y, Points.1.X, ImageHeight)
	, Gdip_DeletePen(pPen)

Loop, % Branches {
	NewPoints := {}
	pPen := Gdip_CreatePen(TreeColor, TrunkWidth)
	for Each, Point in Points {
		N1 := A_Index * 2
			, N2 := (A_Index * 2) + 1
			, NewPoints[N1, "X"] := Point.X + (TrunkLength * Sin(NewPoints[N1, "Angle"] := Point.Angle - Angle))
			, NewPoints[N1, "Y"] := Point.Y - (TrunkLength * Cos(NewPoints[N1].Angle))
			, NewPoints[N2, "X"] := Point.X + (TrunkLength * Sin(NewPoints[N2, "Angle"] := Point.Angle + Angle))
			, NewPoints[N2, "Y"] := Point.Y - (TrunkLength * Cos(NewPoints[N2].Angle))
			, Gdip_DrawLine(G, pPen, Point.X, Point.Y, NewPoints[N1].X, NewPoints[N1].Y)
			, Gdip_DrawLine(G, pPen, Point.X, Point.Y, NewPoints[N2].X, NewPoints[N2].Y)
	}
	TrunkWidth *= Decrease
		, TrunkLength *= Decrease
		, Points := NewPoints
		, Gdip_DeletePen(pPen)
}

Gdip_SaveBitmapToFile(pBitmap, FileOut)
	, Gdip_DisposeImage(pBitmap)
	, Gdip_DeleteGraphics(G)
Run, % FileOut

Exit:
Gdip_Shutdown(pToken)
ExitApp
