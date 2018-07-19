SetBatchlines,-1
;settings
SizeGUI:={w:650,h:400} ;Guisize
pendulum:={length:300,maxangle:90,speed:2,size:30,center:{x:Sizegui.w//2,y:10}} ;pendulum length, size, center, speed and maxangle

pendulum.maxangle:=pendulum.maxangle*0.01745329252
p_Token:=Gdip_Startup()
Gui,+LastFound
Gui,show,% "w" SizeGUI.w  " h" SizeGUI.h
hwnd:=WinActive()
hdc:=GetDC(hwnd)
start:=A_TickCount/1000
G:=Gdip_GraphicsFromHDC(hdc)
pBitmap:=Gdip_CreateBitmap(650, 450)
G2:=Gdip_GraphicsFromImage(pBitmap)
Gdip_SetSmoothingMode(G2, 4)
pBrush := Gdip_BrushCreateSolid(0xff0000FF)
pBrush2 := Gdip_BrushCreateSolid(0xFF777700)
pPen:=Gdip_CreatePenFromBrush(pBrush2, 10)
SetTimer,Update,10

Update:
Gdip_GraphicsClear(G2,0xFFFFFFFF)
time:=start-(A_TickCount/1000*pendulum.speed)
angle:=sin(time)*pendulum.maxangle
x2:=sin(angle)*pendulum.length+pendulum.center.x
y2:=cos(angle)*pendulum.length+pendulum.center.y
Gdip_DrawLine(G2,pPen,pendulum.center.x,pendulum.center.y,x2,y2)
GDIP_DrawCircle(G2,pBrush,pendulum.center.x,pendulum.center.y,15)
GDIP_DrawCircle(G2,pBrush2,x2,y2,pendulum.size)
Gdip_DrawImage(G, pBitmap)
return

GDIP_DrawCircle(g,b,x,y,r){
	Gdip_FillEllipse(g, b, x-r//2,y-r//2 , r, r)
}

GuiClose:
ExitApp
