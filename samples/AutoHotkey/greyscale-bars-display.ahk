h	:= A_ScreenHeight
w	:= A_ScreenWidth
pToken	:= gdip_Startup()
hdc	:= CreateCompatibleDC()
hbm	:= CreateDIBSection(w, h)
obm	:= SelectObject(hdc, hbm)
G	:= Gdip_GraphicsFromHDC(hdc)

OnExit, Exit

Gui +E0x80000 +LastFound +OwnDialogs +Owner +AlwaysOnTop
hwnd	:= WinExist()
Gui Show, NA

columnHeight := h/4

Loop 4
{
	columnY		:= (A_Index-1) * columnHeight
	columnCount	:= 2**(A_Index+2)
	colorgap	:= 255 / (columnCount-1)
	columnWidth	:= w/ColumnCount
	If (A_Index & 1)
		colorComp := 0
	else
		colorComp := 255
		,colorgap *= -1
	MsgBox % colorGap * columnCount
	Loop % columnCount
	{
		columnX := (A_Index-1) * columnWidth
		pBrush := Gdip_BrushCreateSolid(QColor(colorComp, colorComp, colorComp))
		Gdip_FillRectangle(G, pBrush, columnX, columnY, columnWidth, columnHeight)
		Gdip_DeleteBrush(pBrush)
		colorComp += colorgap
	}
	SetFormat, IntegerFast, hex
	SetFormat, IntegerFast, D
}

UpdateLayeredWindow(hwnd, hdc, 0, 0, W, H)

SelectObject(hdc, obm)
DeleteObject(hbm)
DeleteDC(hdc)
Gdip_DeleteGraphics(G)
Return

Esc::
Exit:
Gdip_Shutdown(pToken)
ExitApp

QColor(r, g, b){
	return 0xFF000000 | (r << 16) | (g << 8) | (b)
}
