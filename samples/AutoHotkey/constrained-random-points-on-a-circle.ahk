z=100 ; x = x-coord; y = y-coord; z = count; pBitmap = a pointer to the image; f = filename

pToken	:= Gdip_Startup()
pBitmap := Gdip_CreateBitmap(31, 32)

While z
{
	Random, x, -20, 20
	Random, y, -20,20
	If ( t := sqrt(x**2 + y**2) ) >= 10 && t <= 15
		Gdip_SetPixel(pBitmap, x+15, y+16, 255<<24), z--
}

Gdip_SaveBitmapToFile(pBitmap, f := A_ScriptDir "\ahk_fuzzycircle.png")
run % f

Gdip_DisposeImage(pBitmap)
Gdip_Shutdown(pToken)
