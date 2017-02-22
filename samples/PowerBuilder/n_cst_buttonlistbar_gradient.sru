// The MIT License (MIT)

// Copyright (c) 2016 dario ureña

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// Source: https://github.com/darioaxel/PowerScriptToKDMTransformer/blob/07bf527194b73e133943221dc2199a9442d463eb/resources/inventoryModel/nivel0/nivel1/n_cst_buttonlistbar_gradient.sru

HA$PBExportHeader$n_cst_buttonlistbar_gradient.sru
forward
global type n_cst_buttonlistbar_gradient from nonvisualobject
end type
type gradient_rect from structure within n_cst_buttonlistbar_gradient
end type
type gradient_triangle from structure within n_cst_buttonlistbar_gradient
end type
type rect from structure within n_cst_buttonlistbar_gradient
end type
type trivertex from structure within n_cst_buttonlistbar_gradient
end type
type logfont from structure within n_cst_buttonlistbar_gradient
end type
end forward

type gradient_rect from structure
	unsignedlong		upperleft
	unsignedlong		lowerright
end type

type gradient_triangle from structure
	unsignedlong		vertex1
	unsignedlong		vertex2
	unsignedlong		vertex3
end type

type rect from structure
	long		left
	long		top
	long		right
	long		bottom
end type

type trivertex from structure
	long		x
	long		y
	integer		red
	integer		green
	integer		blue
	integer		alpha
end type

type logfont from structure
	long		lfheight
	long		lfwidth
	long		lfescapement
	long		lforientation
	long		lfweight
	character		lfitalic
	character		lfunderline
	character		lfstrikeout
	character		lfcharset
	character		lfoutprecision
	character		lfclipprecision
	character		lfquality
	character		lfpitchandfamily
	character		lffacename[32]
end type

global type n_cst_buttonlistbar_gradient from nonvisualobject autoinstantiate
end type

type prototypes
FUNCTION ulong SetBkMode(ulong lhdc,ulong nBkMode) LIBRARY "gdi32.dll"
Function Long GetDC (Long hwnd) Library 'user32'
Function Long GetClientRect (Long hwnd, REF RECT lpRect) Library 'user32' alias for "GetClientRect;Ansi"
Function Long ReleaseDC (Long hwnd, Long hdc1) Library 'user32'
Function Boolean GradientRectangle (Long hdc2, TRIVERTEX pVert[], ULong numVert, GRADIENT_RECT pMesh [], ULong numMesh, ULong dMode) Library 'msimg32' Alias For 'GradientFill;Ansi'
Function Boolean GradientTriangle (Long hdc3, TRIVERTEX pVert[], ULong numVert, GRADIENT_TRIANGLE pMesh [], ULong numMesh, ULong dMode) Library 'msimg32' Alias For 'GradientFill;Ansi'

Function ulong Rectangle (ulong hwnd, ulong X1, ulong Y1, ulong X2, ulong Y2) library "gdi32" 
Function ulong CreatePen  (ulong nPenStyle, ulong nWidth, ulong crColor) LIBRARY "Gdi32.dll"
Function ULONG	SelectObject (uLong hdc4, uLong hObject ) LIBRARY "Gdi32.dll"
function boolean DeleteObject(ulong hgdiobject) library "gdi32.dll" 

FUNCTION ulong DrawText(ulong lhdc,ref string lpStr,ulong nCount,ref RECT lpRect,ulong wFormat) LIBRARY "user32.dll" ALIAS FOR "DrawTextA;Ansi"
FUNCTION ulong SetTextColor(ulong lhdc,ulong crColor) LIBRARY "gdi32.dll"
FUNCTION ulong CreateFontIndirect(ref LOGFONT lpLogFont) LIBRARY "gdi32.dll" ALIAS FOR "CreateFontIndirectA;ANSI"

Function ulong RoundRect (uLong hdc5 , uLong X1, ulong Y1 , ulong X2 , ulong Y2 , ulong X3 , ulong Y3 ) LIBRARY "Gdi32.dll"
Function ULONG FillRgn (Long hdc6, Long hRgn , Long hBrush ) LIBRARY "Gdi32.dll"
Function ulong CreateRectRgn (Long X1, Long Y1 , Long X2 , Long Y2 ) LIBRARY "Gdi32.dll"
Function ulong CreateRoundRectRgn (long X1 , long Y1 , long X2 , long Y2 , long X3 , long Y3 ) LIBRARY "Gdi32.dll"

Function ULONG PolyBezier (Long hdc7, REF RECT lpRect, long cPoints ) LIBRARY "Gdi32.dll"
Function ULONG Polyline( Long hdc8, GRADIENT_RECT lppt[], int cPoints) LIBRARY "Gdi32.dll"

FUNCTION boolean Ellipse(ulong hwnd,long x1,long y1,long x2,long y2) LIBRARY "Gdi32.dll"

FUNCTION ulong CreatePolygonRgn(ref POINT ppoint[], int count, int fillMode) Library "GDI32.DLL"
FUNCTION long SelectClipRgn(ulong lhdc, ulong lhrgn) Library "GDI32.DLL"

FUNCTION long PtInRegion(ulong hrgn, int x, int y) LIBRARY "GDI32.dll"

Function boolean ImageList_Draw(long himl, long i, long hdcDst, long lx, long ly, ulong fStyle) Library "comctl32.dll"
Function boolean ImageList_DrawEx(long himl, long i, long hdcDst, long lx, long ly, long lwidth, long lheight, long lback, long lfore , ulong fStyle) Library "comctl32.dll"

end prototypes

type variables
// MS Windows enumerations
CONSTANT ULong GRADIENT_FILL_RECT_H = 0
CONSTANT ULong GRADIENT_FILL_RECT_V = 1
CONSTANT ULong GRADIENT_FILL_TRIANGLE = 2
CONSTANT ULong GRADIENT_FILL_OP_FLAG = 255

CONSTANT Long ILD_TRANSPARENT = 1
CONSTANT Long LVM_GETIMAGELIST = 4098
CONSTANT Long LVSIL_NORMAL = 0
CONSTANT Long ILD_BLEND50 = 4
CONSTANT Long ILD_BLEND25 = 2

// User object enumerations
CONSTANT String TOPLEFT = "TOPLEFT"
CONSTANT String TOPRIGHT = "TOPRIGHT"
CONSTANT String BOTTOMRIGHT = "BOTTOMRIGHT"
CONSTANT String BOTTOMLEFT = "BOTTOMLEFT"

BOOLEAN	ib_displayborder

CONSTANT LONG ALIGN_LEFT = 0
CONSTANT LONG ALIGN_CENTER = 1
CONSTANT LONG ALIGN_RIGHT = 2

CONSTANT LONG DT_WORD_ELLIPSIS = 262144
CONSTANT LONG DT_CALCRECT = 1024
CONSTANT LONG DT_WORDBREAK = 16

Long	il_HDC

Long il_newHeight
Long il_newWidth

PRIVATE:
// Object Handle
Long	HDC

Long	il_ShadowBackColor

ULong iul_font
ULong iul_fontbold

// Dimensions
RECT DC_Rect

// Vertices
TRIVERTEX Corner[4]
TRIVERTEX BorderCorner[2]
TRIVERTEX VistaCorner1[4]
TRIVERTEX VistaCorner2[4]
TRIVERTEX VistaCorner3[4]
end variables

forward prototypes
public subroutine of_verticalgradient (long al_color1, long al_color2)
public subroutine of_splitrgb (long al_color, ref long red, ref long green, ref long blue)
public function boolean of_setdevicecontext (graphicobject ado_palette)
public subroutine of_verticalgradient (long al_color1, long al_color2, graphicobject ado_palette)
public subroutine of_verticalgradient (long al_color1, long al_color2, graphicobject ado_palette, boolean ab_displayborder, long al_bordercolor, integer ai_style)
public subroutine of_xpgradient (long al_color1, long al_color2, graphicobject ado_palette, boolean ab_displayborder, long al_bordercolor)
public function boolean of_setdevicecontext (graphicobject ado_palette, integer ai_style, boolean ab_border)
public function integer of_drawtext (graphicobject ado_palette, string as_text, long al_color, string as_font, long al_size, boolean ab_bold, long al_align, long al_x, long al_y, long al_width, long al_height, boolean ab_displayshadow)
public function integer of_sethdc (long al_hdc)
public function boolean of_getbit (long al_decimal, integer ai_bit)
public function unsignedlong of_createfont (string as_name, long al_size, boolean ab_underline)
public function long of_bitwiseand (long al_value1, long al_value2)
public function long of_bitwiseor (long al_value1, long al_value2)
public function integer of_drawtext (graphicobject ado_palette, string as_text, long al_color, string as_font, long al_size, boolean ab_bold, long al_align, long al_x, long al_y, long al_width, long al_height, boolean ab_displayshadow, boolean ab_elipse, boolean ab_underline, boolean ab_wordwrap)
public function integer of_drawbuttons (dragobject ado_palette, buttons ast_buttons[], long al_bordercolor, long al_imagelist, boolean ab_boldselected, long il_imagesize, long al_height)
public subroutine of_verticalgradient (long al_color1, long al_color2)
public subroutine of_splitrgb (long al_color, ref long red, ref long green, ref long blue)
public function boolean of_setdevicecontext (graphicobject ado_palette)
public subroutine of_verticalgradient (long al_color1, long al_color2, graphicobject ado_palette)
public subroutine of_verticalgradient (long al_color1, long al_color2, graphicobject ado_palette, boolean ab_displayborder, long al_bordercolor, integer ai_style)
public subroutine of_xpgradient (long al_color1, long al_color2, graphicobject ado_palette, boolean ab_displayborder, long al_bordercolor)
public function boolean of_setdevicecontext (graphicobject ado_palette, integer ai_style, boolean ab_border)
public function integer of_drawtext (graphicobject ado_palette, string as_text, long al_color, string as_font, long al_size, boolean ab_bold, long al_align, long al_x, long al_y, long al_width, long al_height, boolean ab_displayshadow)
public function integer of_sethdc (long al_hdc)
public function boolean of_getbit (long al_decimal, integer ai_bit)
public function unsignedlong of_createfont (string as_name, long al_size, boolean ab_underline)
public function long of_bitwiseand (long al_value1, long al_value2)
public function long of_bitwiseor (long al_value1, long al_value2)
public function integer of_drawtext (graphicobject ado_palette, string as_text, long al_color, string as_font, long al_size, boolean ab_bold, long al_align, long al_x, long al_y, long al_width, long al_height, boolean ab_displayshadow, boolean ab_elipse, boolean ab_underline, boolean ab_wordwrap)
public function integer of_drawbuttons (dragobject ado_palette, buttons ast_buttons[], long al_bordercolor, long al_imagelist, boolean ab_boldselected, long il_imagesize, long al_height)
end prototypes

public subroutine of_verticalgradient (long al_color1, long al_color2);Long	ll_Red, &
		ll_Green, &
		ll_Blue, &
		ll_DC
GRADIENT_RECT l_Gradient[1]

LONG hRPen

// Set the colors in the first corner (top left)
of_SplitRGB (al_Color1, ll_Red, ll_Green, ll_Blue)
Corner[1].Red = ll_Red
Corner[1].Green = ll_Green
Corner[1].Blue = ll_Blue

// Set the colors in the third corner (bottom right)
of_SplitRGB (al_Color2, ll_Red, ll_Green, ll_Blue)
Corner[3].Red = ll_Red
Corner[3].Green = ll_Green
Corner[3].Blue = ll_Blue

l_Gradient[1].UpperLeft = 0 // First corner, top left
l_Gradient[1].LowerRight = 2 // Third corner, bottom right

ll_DC = il_HDC//GetDC (HDC)

// Create a border if ib_displayborder is true
IF 	ib_displayborder THEN
	hRPen = CreatePen(0,0,1090519039)
	SelectObject(ll_DC, hRPen)
	Rectangle(ll_DC, Corner[1].X - 1, Corner[1].Y - 1, Corner[3].X +1, Corner[3].Y +1)
END IF

GradientRectangle (ll_DC, Corner, 4, l_Gradient, 1, GRADIENT_FILL_RECT_V)

//ReleaseDC (HDC, ll_DC)
end subroutine

public subroutine of_splitrgb (long al_color, ref long red, ref long green, ref long blue);Red = Mod (al_Color, 256)
Red *= 256

al_Color /= 256
Green	= Mod (al_Color, 256)
Green *= 256

Blue = al_Color / 256
Blue *= 256
end subroutine

public function boolean of_setdevicecontext (graphicobject ado_palette);IF NOT IsValid (ado_Palette) THEN RETURN FALSE

// Get the object's handle
HDC = Handle (ado_Palette)

// Get the object's dimensions
GetClientRect (HDC, DC_RECT)

// let's resize the grandient rectangle if border is displayed
IF ib_displayborder THEN
	// Initialize the vertices
	Corner[1].X = DC_RECT.Left +1 
	Corner[1].Y = DC_RECT.Top +1 
	Corner[2].X = DC_RECT.Right -1
	Corner[2].Y = DC_RECT.Top +1
	Corner[3].X = DC_RECT.Right -1
	Corner[3].Y = DC_RECT.Bottom - 1
	Corner[4].X = DC_RECT.Left +1
	Corner[4].Y = DC_RECT.Bottom - 1
	
	VistaCorner1[1].X = DC_RECT.Left + 1 
	VistaCorner1[1].Y = DC_RECT.Top + 1
	VistaCorner1[2].X = DC_RECT.Right -1
	VistaCorner1[2].Y = DC_RECT.Top + 1
	VistaCorner1[3].X = DC_RECT.Right -1
	VistaCorner1[3].Y = DC_RECT.Bottom - (DC_RECT.Bottom * .65)//20
	VistaCorner1[4].X = DC_RECT.Left +1
	VistaCorner1[4].Y = DC_RECT.Bottom - (DC_RECT.Bottom * .65)//20
	
	VistaCorner3[1].X = DC_RECT.Left + 1 
	VistaCorner3[1].Y = DC_RECT.Top + ((DC_RECT.Bottom * .65) / 2)//10
	VistaCorner3[2].X = DC_RECT.Right -1
	VistaCorner3[2].Y = DC_RECT.Top + ((DC_RECT.Bottom * .65) / 2)//10
	VistaCorner3[3].X = DC_RECT.Right -1
	VistaCorner3[3].Y = DC_RECT.Bottom - 1
	VistaCorner3[4].X = DC_RECT.Left +1
	VistaCorner3[4].Y = DC_RECT.Bottom - 1

ELSE
	Corner[1].X = DC_RECT.Left
	Corner[1].Y = DC_RECT.Top 
	Corner[2].X = DC_RECT.Right
	Corner[2].Y = DC_RECT.Top
	Corner[3].X = DC_RECT.Right
	Corner[3].Y = DC_RECT.Bottom 
	Corner[4].X = DC_RECT.Left
	Corner[4].Y = DC_RECT.Bottom
	
	VistaCorner1[1].X = DC_RECT.Left 
	VistaCorner1[1].Y = DC_RECT.Top
	VistaCorner1[2].X = DC_RECT.Right
	VistaCorner1[2].Y = DC_RECT.Top
	VistaCorner1[3].X = DC_RECT.Right
	VistaCorner1[3].Y = DC_RECT.Bottom - (DC_RECT.Bottom * .65)// - 19
	VistaCorner1[4].X = DC_RECT.Left
	VistaCorner1[4].Y = DC_RECT.Bottom - (DC_RECT.Bottom * .65)// - 19
	
	VistaCorner3[1].X = DC_RECT.Left 
	VistaCorner3[1].Y = DC_RECT.Top + ((DC_RECT.Bottom * .65) / 2) - 1// + 9//12 
	VistaCorner3[2].X = DC_RECT.Right
	VistaCorner3[2].Y = DC_RECT.Top + ((DC_RECT.Bottom * .65) / 2) - 1// + 9//12
	VistaCorner3[3].X = DC_RECT.Right
	VistaCorner3[3].Y = DC_RECT.Bottom
	VistaCorner3[4].X = DC_RECT.Left
	VistaCorner3[4].Y = DC_RECT.Bottom
END IF	

RETURN TRUE


end function

public subroutine of_verticalgradient (long al_color1, long al_color2, graphicobject ado_palette);IF NOT of_SetDeviceContext (ado_Palette) THEN RETURN

of_VerticalGradient (al_Color1, al_Color2)
end subroutine

public subroutine of_verticalgradient (long al_color1, long al_color2, graphicobject ado_palette, boolean ab_displayborder, long al_bordercolor, integer ai_style);CHOOSE CASE ai_STYLE
	CASE 0
		of_XPGradient (al_color1, al_color2, ado_palette,ab_displayborder,al_bordercolor)
END CHOOSE
end subroutine

public subroutine of_xpgradient (long al_color1, long al_color2, graphicobject ado_palette, boolean ab_displayborder, long al_bordercolor);Long	ll_Red, &
		ll_Green, &
		ll_Blue, &
		ll_DC
GRADIENT_RECT l_Gradient[1]

LONG hRPen

ib_displayborder = ab_displayborder

IF NOT of_SetDeviceContext (ado_Palette) THEN RETURN

//of_VerticalGradient (al_Color1, al_Color2)

// Set the colors in the first corner (top left)
of_SplitRGB (al_Color1, ll_Red, ll_Green, ll_Blue)
Corner[1].Red = ll_Red
Corner[1].Green = ll_Green
Corner[1].Blue = ll_Blue

// Set the colors in the third corner (bottom right)
of_SplitRGB (al_Color2, ll_Red, ll_Green, ll_Blue)
Corner[3].Red = ll_Red
Corner[3].Green = ll_Green
Corner[3].Blue = ll_Blue

l_Gradient[1].UpperLeft = 0 // First corner, top left
l_Gradient[1].LowerRight = 2 // Third corner, bottom right

ll_DC = il_HDC//GetDC (HDC)

// Create a border if ib_displayborder is true
IF 	ab_displayborder THEN
	hRPen = CreatePen(0,0,al_bordercolor)
	SelectObject(ll_DC, hRPen)
	Rectangle(ll_DC, Corner[1].X - 1, Corner[1].Y - 1, Corner[3].X +1, Corner[3].Y +1)
	DeleteObject(hRPen)
END IF

GradientRectangle (ll_DC, Corner, 4, l_Gradient, 1, GRADIENT_FILL_RECT_V)

//ReleaseDC (HDC, ll_DC)
end subroutine

public function boolean of_setdevicecontext (graphicobject ado_palette, integer ai_style, boolean ab_border);IF NOT IsValid (ado_Palette) THEN RETURN FALSE

// Get the object's handle
HDC = Handle (ado_Palette)

// Get the object's dimensions
GetClientRect (HDC, DC_RECT)

// let's resize the grandient rectangle if border is displayed
choose case ai_style
	case 2
	IF ab_border THEN
		// Initialize the vertices
		Corner[1].X = DC_RECT.Left +1 
		Corner[1].Y = DC_RECT.Top +1 
		Corner[2].X = DC_RECT.Right -1
		Corner[2].Y = DC_RECT.Top +1
		Corner[3].X = DC_RECT.Right -1
		Corner[3].Y = DC_RECT.Bottom - 1
		Corner[4].X = DC_RECT.Left +1
		Corner[4].Y = DC_RECT.Bottom - 1
	
		VistaCorner1[1].X = DC_RECT.Left + 2 
		VistaCorner1[1].Y = DC_RECT.Top + 2
		VistaCorner1[2].X = DC_RECT.Right - 1
		VistaCorner1[2].Y = DC_RECT.Top + 2
		VistaCorner1[3].X = DC_RECT.Right -2
		VistaCorner1[3].Y = DC_RECT.Bottom - (DC_RECT.Bottom * .48)//14
		VistaCorner1[4].X = DC_RECT.Left +1
		VistaCorner1[4].Y = DC_RECT.Bottom - (DC_RECT.Bottom * .48)//14
	
		VistaCorner3[1].X = DC_RECT.Left + 2
		VistaCorner3[1].Y = DC_RECT.Top + (DC_RECT.Bottom * .53)//16
		VistaCorner3[2].X = DC_RECT.Right //-1
		VistaCorner3[2].Y = DC_RECT.Top + (DC_RECT.Bottom * .53)//16 
		VistaCorner3[3].X = DC_RECT.Right - 2
		VistaCorner3[3].Y = DC_RECT.Bottom - 2
		VistaCorner3[4].X = DC_RECT.Left 
		VistaCorner3[4].Y = DC_RECT.Bottom - 2
	ELSE
		Corner[1].X = DC_RECT.Left +1 
		Corner[1].Y = DC_RECT.Top +1 
		Corner[2].X = DC_RECT.Right -1
		Corner[2].Y = DC_RECT.Top +1
		Corner[3].X = DC_RECT.Right -1
		Corner[3].Y = DC_RECT.Bottom - 1
		Corner[4].X = DC_RECT.Left +1
		Corner[4].Y = DC_RECT.Bottom - 1
	
		VistaCorner1[1].X = DC_RECT.Left
		VistaCorner1[1].Y = DC_RECT.Top 
		VistaCorner1[2].X = DC_RECT.Right
		VistaCorner1[2].Y = DC_RECT.Top 
		VistaCorner1[3].X = DC_RECT.Right
		VistaCorner1[3].Y = DC_RECT.Bottom - (DC_RECT.Bottom * .48)//14
		VistaCorner1[4].X = DC_RECT.Left
		VistaCorner1[4].Y = DC_RECT.Bottom - (DC_RECT.Bottom * .48)//14
	
		VistaCorner3[1].X = DC_RECT.Left
		VistaCorner3[1].Y = DC_RECT.Top + (DC_RECT.Bottom * .53)//16
		VistaCorner3[2].X = DC_RECT.Right 
		VistaCorner3[2].Y = DC_RECT.Top + (DC_RECT.Bottom * .53)//16 
		VistaCorner3[3].X = DC_RECT.Right
		VistaCorner3[3].Y = DC_RECT.Bottom
		VistaCorner3[4].X = DC_RECT.Left 
		VistaCorner3[4].Y = DC_RECT.Bottom
	END IF
end choose

RETURN TRUE


end function

public function integer of_drawtext (graphicobject ado_palette, string as_text, long al_color, string as_font, long al_size, boolean ab_bold, long al_align, long al_x, long al_y, long al_width, long al_height, boolean ab_displayshadow);RETURN of_DrawText (ado_palette, as_text, al_color, as_font, al_size, ab_bold, al_align, al_x, al_y, al_width, al_height, ab_displayshadow, FALSE, FALSE, FALSE)
end function

public function integer of_sethdc (long al_hdc);il_HDC = al_hdc

RETURN 1
end function

public function boolean of_getbit (long al_decimal, integer ai_bit);Boolean lb_null

//Check parameters
If IsNull(al_decimal) or IsNull(ai_bit) then
	SetNull(lb_null)
	Return lb_null
End If

//Assumption ai_bit is the nth bit counting right to left with
//the leftmost bit being bit one.
//al_decimal is a binary number as a base 10 long.
If Int(Mod(al_decimal / (2 ^(ai_bit - 1)), 2)) > 0 Then
	Return True
End If

Return False
end function

public function unsignedlong of_createfont (string as_name, long al_size, boolean ab_underline);LogFont lstr_Lf
uLong lul_Font

lstr_Lf.lffacename = as_name
lstr_Lf.lfweight = 400
lstr_Lf.lfheight = al_size * -1
lstr_Lf.lfPitchAndFamily = '1'
lstr_Lf.lfClipPrecision = Char(2)
lstr_Lf.lfOutPrecision = Char(1)
lstr_Lf.lfQuality = Char(1)
lstr_Lf.lfCharset = Char(1)

IF ab_underline THEN
	lstr_Lf.lfunderline = Char(1)
END IF

iul_font = CreateFontIndirect( lstr_Lf )
lstr_Lf.lfweight = 700
iul_fontbold = CreateFontIndirect( lstr_Lf )

RETURN 1
end function

public function long of_bitwiseand (long al_value1, long al_value2);Integer		li_Cnt
Long			ll_Result
Boolean		lb_Value1[32], lb_Value2[32]

// Check for nulls
If IsNull(al_Value1) Or IsNull(al_Value2) Then
	SetNull(ll_Result)
	Return ll_Result
End If

// Get all bits for both values
For li_Cnt = 1 To 32
	lb_Value1[li_Cnt] = of_getbit(al_Value1, li_Cnt)
	lb_Value2[li_Cnt] = of_getbit(al_Value2, li_Cnt)
Next

// And them together
For li_Cnt = 1 To 32
	If lb_Value1[li_Cnt] And lb_Value2[li_Cnt] Then
		ll_Result = ll_Result + (2^(li_Cnt - 1))
	End If
Next

Return ll_Result
end function

public function long of_bitwiseor (long al_value1, long al_value2);Integer		li_Cnt
Long			ll_Result
Boolean		lb_Value1[32], lb_Value2[32]

// Check for nulls
If IsNull(al_Value1) Or IsNull(al_Value2) Then
	SetNull(ll_Result)
	Return ll_Result
End If

// Get all bits for both values
For li_Cnt = 1 To 32
	lb_Value1[li_Cnt] = of_getbit(al_Value1, li_Cnt)
	lb_Value2[li_Cnt] = of_getbit(al_Value2, li_Cnt)
Next

// Or them together
For li_Cnt = 1 To 32
	If lb_Value1[li_Cnt] Or lb_Value2[li_Cnt] Then
		ll_Result = ll_Result + (2^(li_Cnt - 1))
	End If
Next

Return ll_Result

end function

public function integer of_drawtext (graphicobject ado_palette, string as_text, long al_color, string as_font, long al_size, boolean ab_bold, long al_align, long al_x, long al_y, long al_width, long al_height, boolean ab_displayshadow, boolean ab_elipse, boolean ab_underline, boolean ab_wordwrap);Long	ll_Red, &
		ll_Green, &
		ll_Blue, &
		ll_DC, ll_parm, ll_sizeparm
GRADIENT_RECT l_Gradient[1]
RECT l_Rect, l_Rectback

LONG hRPen

IF Len(Trim(as_text)) = 0 THEN
	il_newWidth = 1
	il_NewHeight = al_height
	RETURN 0
END IF

IF NOT of_SetDeviceContext (ado_Palette) THEN RETURN 0

ll_DC = il_HDC//GetDC (HDC)

of_CreateFont(as_font, al_size, ab_underline)

IF ab_bold THEN
	SelectObject(ll_DC, iul_fontbold)
ELSE
	SelectObject(ll_DC, iul_font)
END IF

IF ab_elipse THEN
	ll_sizeparm = of_BitWiseOr(al_align, DT_WORD_ELLIPSIS)
ELSE
	IF ab_wordwrap THEN
		ll_sizeparm = of_BitWiseOr(al_align, DT_WORDBREAK)
	END IF
END IF

IF ab_wordwrap THEN
	ll_parm = of_BitWiseOr(DT_CALCRECT, DT_WORDBREAK)
ELSE
	ll_parm = DT_CALCRECT
END IF

SetBkMode(ll_DC, 1)

/*-------------------------------------------------------------------
	Normal text
-------------------------------------------------------------------*/
// Position
l_Rect.Left = al_x
l_Rect.Top = al_y
l_Rect.Right = al_width
l_Rect.Bottom = al_height

//SetBkMode(ll_DC, 1)
SetTextColor(ll_DC, al_color)

IF NOT ab_elipse THEN
	Drawtext(ll_DC, as_text, LEN(as_text), l_Rect, ll_parm)
END IF

IF al_align = ALIGN_RIGHT OR &
   al_align = ALIGN_CENTER THEN
	l_Rect.Right = al_width
END IF

Drawtext(ll_DC, as_text, LEN(as_text), l_Rect, ll_sizeparm)

il_NewHeight = l_Rect.Bottom
il_newWidth = l_Rect.RIGHT

DeleteObject(iul_font)
DeleteObject(iul_fontbold)

//ReleaseDC (HDC, ll_DC)

RETURN 1
end function

public function integer of_drawbuttons (dragobject ado_palette, buttons ast_buttons[], long al_bordercolor, long al_imagelist, boolean ab_boldselected, long il_imagesize, long al_height);point lp[], lp_line[]
point lp_empty[]
gradient_rect l_Gradient[1]
GRADIENT_RECT l_Line[]
rect rc
Long	ll_Red, &
		ll_Green, &
		ll_Blue
long hRgn, hRPen, ll_index, ll_count, ll_textcolor, ll_parm
Long ll_inner, ll_innercount
Long ll_textx, ll_texty, ll_textwidth, ll_y
Boolean lb_bold

ll_count = UpperBound(ast_buttons)

FOR ll_index = 1 TO ll_count
	lp = lp_empty
	
	lp = ast_buttons[ll_index].ast_point
	
	IF lp[4].py < 0 OR lp[1].py > al_height THEN
		CONTINUE
	END IF
	
	l_Gradient[1].UpperLeft = 0
	l_Gradient[1].LowerRight = 2
	
	IF ast_buttons[ll_index].ab_selected OR &
	   ast_buttons[ll_index].ab_mouseover THEN
		Corner[1].X = lp[1].px 
		Corner[1].Y = lp[1].py
		Corner[2].X = lp[2].px 
		Corner[2].Y = lp[2].py
		Corner[3].X = lp[3].px 
		Corner[3].Y = lp[3].py
		Corner[4].X = lp[4].px 
		Corner[4].Y = lp[4].py
		
		// Set the colors in the first corner (top left)
		of_SplitRGB (ast_buttons[ll_index].al_backcolor1, ll_Red, ll_Green, ll_Blue)
		Corner[1].Red = ll_Red
		Corner[1].Green = ll_Green
		Corner[1].Blue = ll_Blue
		
		// Set the colors in the third corner (bottom right)
		of_SplitRGB (ast_buttons[ll_index].al_backcolor2, ll_Red, ll_Green, ll_Blue)
		Corner[3].Red = ll_Red
		Corner[3].Green = ll_Green
		Corner[3].Blue = ll_Blue
		
		//Create Polygon
		hRPen = CreatePen(0,0,al_bordercolor)
		SelectObject(il_HDC, hRPen)
		hRgn = CreateRoundRectRgn( Corner[1].X, Corner[1].Y, Corner[3].X, Corner[3].Y,7,7)
		RoundRect( il_HDC, Corner[1].X - 1, Corner[1].Y - 1, Corner[3].X, Corner[3].Y,10,10)
		
		SelectClipRgn(il_HDC, hRgn)
		
		//Gradient Fill
		GradientRectangle (il_HDC, Corner, 4, l_Gradient, 1, GRADIENT_FILL_RECT_V)
		
		SelectClipRgn(il_HDC, 0)
		
		DeleteObject(hRPen)
		DeleteObject(hRgn)
	END IF
	
	IF ast_buttons[ll_index].ab_enabled THEN
		ll_textcolor = 0
	ELSE
		ll_textcolor = RGB(100,100,100)
	END IF
	
	IF ab_boldselected AND ast_buttons[ll_index].ab_selected THEN
		lb_bold = TRUE
	ELSE
		lb_bold = FALSE
	END IF
	
	of_DrawText(ado_palette, &
	            ast_buttons[ll_index].as_text, &
					ll_textcolor, &
					'tahoma', &
					11, lb_bold, ALIGN_CENTER, &
					ast_buttons[ll_index].ast_point[1].px, &
					ast_buttons[ll_index].ast_point[1].py + il_imagesize + 6, &
					ast_buttons[ll_index].ast_point[2].px - ast_buttons[ll_index].ast_point[1].px + 10, &
					ast_buttons[ll_index].al_textheight, &
					FALSE, FALSE, FALSE, TRUE)
	
	//----------------------------------------
	
	IF ast_buttons[ll_index].ab_enabled THEN
		ImageList_DrawEx(al_imagelist, &
		                 ast_buttons[ll_index].al_image - 1, &
							  il_HDC, &
							  ast_buttons[ll_index].ast_point[1].px + (ast_buttons[ll_index].ast_point[2].px - ast_buttons[ll_index].ast_point[1].px) / 2 - (il_imagesize / 2) , &
							  ast_buttons[ll_index].ast_point[1].py + 4, &
							  il_imagesize,il_imagesize, &
							  0, 0, ILD_TRANSPARENT )
	ELSE
		ll_parm = of_BitWiseOR(ILD_TRANSPARENT, ILD_BLEND50)
		ImageList_DrawEx(al_imagelist, &
		                 ast_buttons[ll_index].al_image - 1, &
							  il_HDC, &
							  ast_buttons[ll_index].ast_point[1].px + (ast_buttons[ll_index].ast_point[2].px - ast_buttons[ll_index].ast_point[1].px) / 2 - (il_imagesize / 2) , &
							  ast_buttons[ll_index].ast_point[1].py + 4, &
							  il_imagesize,il_imagesize, &
							  4294967295, RGB(128,128,128), ll_parm )
	END IF
	
NEXT

RETURN 1
end function

on n_cst_buttonlistbar_gradient.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_cst_buttonlistbar_gradient.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

