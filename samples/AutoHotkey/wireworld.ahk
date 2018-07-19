#SingleInstance, Force
#NoEnv
SetBatchLines, -1
File := "Wireworld.txt"
CellSize := 20
CellSize2 := CellSize - 2
C1 := 0xff000000
C2 := 0xff0066ff
C3 := 0xffd40055
C4 := 0xffffcc00

if (!FileExist(File)) {
	MsgBox, % "File(" File ") is not present."
	ExitApp
}

; Uncomment if Gdip.ahk is not in your standard library
; #Include, Gdip.ahk
If !pToken := Gdip_Startup(){
	MsgBox, 48, Gdiplus error!, Gdiplus failed to start. Please ensure you have Gdiplus on your system.
	ExitApp
}
OnExit, Exit

A := [], Width := 0
Loop, Read, % File
{
	Row := A_Index
	Loop, Parse, A_LoopReadLine
	{
		if (A_Index > Width)
			Width := A_Index
		if (A_LoopField = A_Space)
			continue
		A[Row, A_Index] := A_LoopField
	}
}

Width := Width * CellSize + 2 * CellSize
, Height := Row * CellSize + 2 * CellSize
, Row := ""
, TopLeftX := (A_ScreenWidth - Width) // 2
, TopLeftY := (A_ScreenHeight - Height) // 2

Gui, 1: -Caption +E0x80000 +LastFound +AlwaysOnTop +ToolWindow +OwnDialogs
Gui, 1: Show, NA

hwnd1 := WinExist()
, hbm := CreateDIBSection(Width, Height)
, hdc := CreateCompatibleDC()
, obm := SelectObject(hdc, hbm)
, G := Gdip_GraphicsFromHDC(hdc)
, Gdip_SetSmoothingMode(G, 4)

Loop {
	pBrush := Gdip_BrushCreateSolid(C1)
	, Gdip_FillRectangle(G, pBrush, 0, 0, Width, Height)
	, Gdip_DeleteBrush(pBrush)

	for RowNum, Row in A
		for CellNum, Cell in Row
			C := Cell = "H" ? C2 : Cell = "t" ? C3 : C4
			, pBrush := Gdip_BrushCreateSolid(C)
			, Gdip_FillRectangle(G, pBrush, CellNum * CellSize + 1, RowNum * CellSize - 2, CellSize2, CellSize2)
			, Gdip_DeleteBrush(pBrush)
	

	UpdateLayeredWindow(hwnd1, hdc, TopLeftX, TopLeftY, Width, Height)
	, Gdip_GraphicsClear(G)
	, A := NextState(A)
	Sleep, 600
}

NextState(A) {
	B := {}
	for RowNum, Row in A {
		for CellNum, Cell in Row {
			if (Cell = "H")
				B[RowNum, CellNum] := "t"
			else if (Cell = "t")
				B[RowNum, CellNum] := "."
			else if (Cell = ".") {
				H_Count := 0
				Loop 3 {
					Y := RowNum - 2 + A_Index
					Loop, 3 {
						X := CellNum - 2 + A_Index
						if (A[Y, X] = "H")
							H_Count++
					}
				}
				if (H_Count = 1 || H_Count = 2)
					B[RowNum, CellNum] := "H"
				else
					B[RowNum, CellNum] := "."
			}
		}
	}
	return B
}

p::Pause

Esc::
Exit:
Gdip_Shutdown(pToken)
ExitApp
