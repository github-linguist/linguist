Stripped(x){
	Loop Parse, x
		if Asc(A_LoopField) > 31 and Asc(A_LoopField) < 128
			r .= A_LoopField
	return r
}
MsgBox % stripped("`ba" Chr(00) "b`n`rc`fd" Chr(0xc3))
