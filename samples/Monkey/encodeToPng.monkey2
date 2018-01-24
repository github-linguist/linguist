
#Import "<std>"
Using std..

'Set your own path here. Defaults to build folder.
Global path:= AppDir() + "encodeToPng.png"		

Function Main()
	
	'Write from PixMap
	Local source := New Pixmap( 100, 100 )
	For Local y := 0 Until source.Width
		For Local x := 0 Until source.Height
			'Generates random pixels
			source.SetPixelARGB( x, y, ARGB( 255, Rnd(0, 255), 0, Rnd(0, 255) ) )
		Next
	Next
	source.Save( path )
	
	'Read from png to PixMap
	Local dest := Pixmap.Load( path )
	Local a :=  ""
	Local r :=  ""
	Local g :=  ""
	Local b :=  ""
	For Local y := 0 Until dest.Width
		For Local x := 0 Until source.Height
			Local argb := dest.GetPixelARGB(x,y) 
			a += ARGBToAlpha( argb ) + " "
			r += ARGBToRed( argb ) + " "
			g += ARGBToGreen( argb ) + " "
			b += ARGBToBlue( argb ) + " "
		Next
		a += "~n"
		r += "~n"
		g += "~n"
		b += "~n"
	Next
	
	'Print resulting pixels
	Print( " ~nAlpha:~n" + a )
	Print( " ~nRed:~n" + r )
	Print( " ~nGreen:~n" + g )
	Print( " ~nBlue:~n" + b )	
	
End


'**************** Color Functions **************** 


Function ARGB:UInt( a:Float, r:Float, g:Float, b:Float )
	Assert ( a<=1.0, "Alpha max value is 1.0" )
	Assert ( r<=1.0, "Red max value is 1.0" )
	Assert ( g<=1.0, "Green max value is 1.0" )
	Assert ( b<=1.0, "Blue max value is 1.0" )
	Return UInt(a*255) Shl 24 | UInt(r*255) Shl 16 | UInt(g*255) Shl 8 | UInt(b*255)
End

Function ARGB:UInt( a:Int, r:Int, g:Int, b:Int )
	Assert ( a<256, "Alpha can't be higher than 255" )
	Assert ( r<256, "Red can't be higher than 255" )
	Assert ( g<256, "Green can't be higher than 255" )
	Assert ( b<256, "Blue can't be higher than 255" )
	Return( a Shl 24 | r Shl 16 | g Shl 8 | b )
End

Function ARGBToAlpha:Int( argb:UInt )
	Return argb Shr 24 & $ff
End

Function ARGBToRed:Int( argb:UInt )
	Return argb Shr 16 & $ff
End

Function ARGBToGreen:Int( argb:UInt )
	Return argb Shr 8 & $ff
End

Function ARGBToBlue:Int( argb:UInt )
	Return argb & $ff
End

