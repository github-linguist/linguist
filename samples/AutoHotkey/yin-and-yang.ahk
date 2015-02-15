Yin_and_Yang(50,  50, A_ScriptDir "\YinYang1.png")
Yin_and_Yang(300, 300,A_ScriptDir "\YinYang2.png")

Yin_and_Yang(width, height, fileName
	, color1=0xFFFFFFFF, color2=0xFF000000, outlineWidth=1){

	pToken 	 := gdip_Startup()
	pBitmap	 := gdip_CreateBitmap(w := width, h := height)
	w-=1, h-=1
	pGraphics:= gdip_GraphicsFromImage(pBitmap)
	pBrushW	 := gdip_BrushCreateSolid(color1)
	pBrushB	 := gdip_BrushCreateSolid(color2)

	gdip_SetSmoothingMode(pGraphics, 4) 			; Antialiasing

	If (outlineWidth){
		pPen := gdip_CreatePen(0xFF000000, outlineWidth)
		gdip_DrawEllipse(pGraphics, pPen, 0, 0, w, h)
		gdip_DeletePen(pPen)
	}

	gdip_FillPie(pGraphics, pBrushB, 0, 0, w, h, -90, 180)
	gdip_FillPie(pGraphics, pBrushW, 0, 0, w, h,  90, 180)
	gdip_FillEllipse(pGraphics, pBrushB, w//4, h//2, w//2, h//2)
	gdip_FillEllipse(pGraphics, pBrushW, w//4, 0   , w//2, h//2)
	gdip_FillEllipse(pGraphics, pBrushB, 5*w//12, h//6, w//6, h//6)
	gdip_FillEllipse(pGraphics, pBrushW, 5*w//12, 4*h//6,w//6,h//6)

	r := gdip_SaveBitmapToFile(pBitmap, filename)

	; cleanup:
	gdip_DeleteBrush(pBrushW), gdip_deleteBrush(pBrushB)
	gdip_DisposeImage(pBitmap)
	gdip_DeleteGraphics(pGraphics)
	gdip_Shutdown(pToken)
	return r
}
