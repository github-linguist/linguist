h	:= A_ScreenHeight
w	:= A_ScreenWidth
pToken	:= Gdip_Startup()
hdc	:= CreateCompatibleDC()
hbm	:= CreateDIBSection(w, h)
obm	:= SelectObject(hdc, hbm)
G	:= Gdip_GraphicsFromHDC(hdc)

OnExit, Exit

Gui -Caption +E0x80000 +LastFound +OwnDialogs +Owner +AlwaysOnTop
Gui Show, NA
hwnd	:= WinExist()

pBrushB := Gdip_BrushCreateSolid(0xFF000000)
pBrushW := Gdip_BrushCreateSolid(0xFFFFFFFF)
Loop 4
{
	n := A_Index
	Loop % w
		BorW := A_Index & 1 ? "B" : "W"
		,Gdip_FillRectangle(G, pBrush%BorW%
				    , A_Index*n-n, (n-1)*h/4, n, h/4)
}

UpdateLayeredWindow(hwnd, hdc, 0, 0, W, H)

Gdip_DeleteBrush(pBrushB)
Gdip_DeleteBrush(pBrushW)

SelectObject(hdc, obm)
DeleteObject(hbm)
DeleteDC(hdc)
Gdip_DeleteGraphics(G)
Return

Escape::
Exit:
Gdip_Shutdown(pToken)
ExitApp
