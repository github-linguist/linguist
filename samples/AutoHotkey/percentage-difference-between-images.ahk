startup()
dibSection := getPixels("lenna100.jpg")
dibSection2 := getPixels("lenna50.jpg") ; ("File-Lenna100.jpg")
pixels := dibSection.pBits
pixels2 := dibSection2.pBits
z := 0
loop % dibSection.width * dibSection.height * 4
{
x := numget(pixels - 1, A_Index, "uchar")
y := numget(pixels2 - 1, A_Index, "uchar")
z += abs(y - x)
}
msgbox % z / (dibSection.width * dibSection.height * 3 * 255 / 100 ) ; 1.626
return

CreateDIBSection2(hDC, nW, nH, bpp = 32, ByRef pBits = "")
{
dib := object()
	NumPut(VarSetCapacity(bi, 40, 0), bi)
	NumPut(nW, bi, 4)
	NumPut(nH, bi, 8)
	NumPut(bpp, NumPut(1, bi, 12, "UShort"), 0, "Ushort")
	NumPut(0,  bi,16)
hbm := DllCall("gdi32\CreateDIBSection", "Uint", hDC, "Uint", &bi, "Uint", 0, "UintP", pBits, "Uint", 0, "Uint", 0)

dib.hbm := hbm
dib.pBits := pBits
dib.width := nW
dib.height := nH
dib.bpp := bpp
dib.header := header
	Return	dib
}



startup()
{
global disposables
disposables := object()
disposables.pBitmaps := object()
disposables.hBitmaps := object()

If !(disposables.pToken := Gdip_Startup())
{
	MsgBox, 48, gdiplus error!, Gdiplus failed to start. Please ensure you have gdiplus on your system
	ExitApp
}
  OnExit, gdipExit
}


gdipExit:
loop % disposables.hBitmaps._maxindex()
DllCall("DeleteObject", "Uint", disposables.hBitmaps[A_Index])
Gdip_Shutdown(disposables.pToken)
ExitApp

getPixels(imageFile)
{
global disposables ; hBitmap will be disposed later
pBitmapFile1 := Gdip_CreateBitmapFromFile(imageFile)
hbmi := Gdip_CreateHBITMAPFromBitmap(pBitmapFile1)
width := Gdip_GetImageWidth(pBitmapFile1)
height := Gdip_GetImageHeight(pBitmapFile1)

	mDCo := DllCall("CreateCompatibleDC", "Uint", 0)
	mDCi := DllCall("CreateCompatibleDC", "Uint", 0)
	dibSection := CreateDIBSection2(mDCo, width, height)
	hBMo := dibSection.hbm

	oBM := DllCall("SelectObject", "Uint", mDCo, "Uint", hBMo)
	iBM := DllCall("SelectObject", "Uint", mDCi, "Uint", hbmi)

	DllCall("BitBlt", "Uint", mDCo, "int", 0, "int", 0, "int", width, "int", height, "Uint", mDCi, "int", 0, "int", 0, "Uint", 0x40000000 | 0x00CC0020)

	DllCall("SelectObject", "Uint", mDCo, "Uint", oBM)
DllCall("ReleaseDC", "Uint", 0, "Uint", mDCi)
DllCall("ReleaseDC", "Uint", 0, "Uint", mDCo)
Gdip_DisposeImage(pBitmapFile1)
DllCall("DeleteObject", "Uint", hBMi)
disposables.hBitmaps._insert(hBMo)
return dibSection
}
#Include Gdip.ahk  ; Thanks to tic (Tariq Porter) for his GDI+ Library
