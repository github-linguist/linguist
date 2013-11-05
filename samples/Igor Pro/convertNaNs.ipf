#pragma rtGlobals=1		// Use modern global access method.

Function convertNaNs(w, NaNVal)
	wave w
	variable NaNVal
	
	variable i
	For(i=0; i<(NumPnts(w)); i = i+1)
		if((NumType(w[i])) == 2)
			w[i] = NaNVal
		endif
	Endfor
	
End