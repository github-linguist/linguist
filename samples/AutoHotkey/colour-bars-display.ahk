#SingleInstance, Force
#NoEnv
SetBatchLines, -1

; Uncomment if Gdip.ahk is not in your standard library
;#Include, Gdip.ahk

; Start gdi+
If !pToken := Gdip_Startup()
{
   message =
   ( LTrim
      gdiplus error!, Gdiplus failed to start.
      Please ensure you have gdiplus on your system.
   )
   MsgBox, 48, %message%
   ExitApp
}
OnExit, Exit

; Set the width and height we want as our drawing area, to draw everything in.
; This will be the dimensions of our bitmap
Width := A_ScreenWidth, Height := A_ScreenHeight

; Create a layered window
; (+E0x80000 : must be used for UpdateLayeredWindow to work!)
; that is always on top (+AlwaysOnTop), has no taskbar entry or caption
Gui, 1: -Caption +E0x80000 +LastFound +OwnDialogs +Owner +AlwaysOnTop

; Show the window
Gui, 1: Show, NA

; Get a handle to this window we have created in order to update it later
hwnd1 := WinExist()

; Create a gdi bitmap with width and height of what we are going to
; draw into it. This is the entire drawing area for everything
hbm := CreateDIBSection(Width, Height)

; Get a device context compatible with the screen
hdc := CreateCompatibleDC()

; Select the bitmap into the device context
obm := SelectObject(hdc, hbm)

; Get a pointer to the graphics of the bitmap, for use with drawing functions
G := Gdip_GraphicsFromHDC(hdc)

; ARGB = Transparency, Red, Green, Blue
Colors := "0xFF000000,0xFFFF0000,0xFF00FF00,0xFF0000FF"
Colors .= ",0xFFFF00FF,0xFF00FFFF,0xFFFFFF00,0xFFFFFFFF"
; This list ^ is Black, Red, Green, Blue, Magenta, Cyan, Yellow, White
StringSplit Colors, Colors, `,
w := Width // Colors0
Loop % Colors0
{
   ; Create a brush to draw a rectangle
   pBrush := Gdip_BrushCreateSolid(Colors%A_Index%)

   ; Fill the graphics of the bitmap with a rectangle using the brush created
   Gdip_FillRectangle(G, pBrush, w*(A_Index-1), 0, w, height)

   ; Delete the brush as it is no longer needed and wastes memory
   Gdip_DeleteBrush(pBrush)
}
; Update the specified window we have created (hwnd1) with a handle to our
; bitmap (hdc), specifying the x,y,w,h we want it positioned on our screen
; So this will position our gui at (0,0) with the Width and
; Height specified earlier
UpdateLayeredWindow(hwnd1, hdc, 0, 0, Width, Height)


; Select the object back into the hdc
SelectObject(hdc, obm)

; Now the bitmap may be deleted
DeleteObject(hbm)

; Also the device context related to the bitmap may be deleted
DeleteDC(hdc)

; The graphics may now be deleted
Gdip_DeleteGraphics(G)
Return

;#######################################################################

GuiEscape:
Exit:
; gdi+ may now be shutdown on exiting the program
Gdip_Shutdown(pToken)
ExitApp
Return
