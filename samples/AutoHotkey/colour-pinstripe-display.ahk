h	:= A_ScreenHeight
w	:= A_ScreenWidth
pToken	:= gdip_Startup()
hdc	:= CreateCompatibleDC()
hbm	:= CreateDIBSection(w, h)
obm	:= SelectObject(hdc, hbm)
G	:= Gdip_GraphicsFromHDC(hdc)

OnExit, Exit

Gui -Caption +E0x80000 +LastFound +OwnDialogs +Owner +AlwaysOnTop
Gui Show, NA
hwnd	:= WinExist()

colors	:= [0xFF000000, 0xFFFF0000, 0xFF00FF00, 0xFF0000FF
	  , 0xFFFF00FF, 0xFF00FFFF, 0xFFFFFF00, 0xFFFFFFFF] ; ARGB
pBrush	:= []
Loop % colors.MaxIndex()
	pBrush[A_Index] := Gdip_BrushCreateSolid(colors[A_Index])
Loop 4
{
	n := A_Index
	Loop % w
		Gdip_FillRectangle(G, pBrush[Mod(A_Index-1, colors.MaxIndex())+1]
				    , A_Index*n-n, (n-1)*h/4, n, h/4)
}


UpdateLayeredWindow(hwnd, hdc, 0, 0, W, H)

Loop % colors.MaxIndex()
	Gdip_DeleteBrush(pBrush[A_Index])

SelectObject(hdc, obm)
DeleteObject(hbm)
DeleteDC(hdc)
Gdip_DeleteGraphics(G)
Return

Esc::
Exit:
Gdip_Shutdown(pToken)
ExitApp
