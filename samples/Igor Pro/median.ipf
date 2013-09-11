#pragma rtGlobals=1		// Use modern global access method.

Function median(w)
	wave w
	
	extract /o w, temp, numtype(w) != 2
	sort temp, temp
	wavestats /q temp
	print num2str(V_avg) + " " + num2str(temp[V_npnts*0.5]) + " " + num2str(temp[V_npnts*0.75]) + " " + num2str(temp[V_npnts*0.25])
	killwaves temp
End

Function/D Median2(w) // Returns median value of wave w
	Wave w
	//variable x1, x2
	Variable result, i
	Duplicate/O w, temp // Make a clone of wave
	for(i=0; i < (numpnts(temp)); i +=1)
		if(Numtype(temp[i])==2)
			DeletePoints i, 1, temp
			i -= 1
		endif
	endfor
	Sort temp, temp // Sort clone	
	result = temp[numpnts(temp)*0.05]
	KillWaves temp // Kill clone
	return result
End