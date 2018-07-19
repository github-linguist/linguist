Function Bitwise
	Push $0
	Push $1
	Push $2
	StrCpy $0 7
	StrCpy $1 2
	
	IntOp $2 $0 & $1
	DetailPrint "Bitwise AND: $0 & $1 = $2"
	IntOp $2 $0 | $1
	DetailPrint "Bitwise OR: $0 | $1 = $2"
	IntOp $2 $0 ^ $1
	DetailPrint "Bitwise XOR: $0 ^ $1 = $2"
	IntOp $2 $0 ~
	DetailPrint "Bitwise NOT (negate in NSIS docs): ~$0 = $2"
	DetailPrint "There are no Arithmetic shifts in NSIS"
	IntOp $2 $0 >> $1
	DetailPrint "Right Shift: $0 >> 1 = $2"
	IntOp $2 $0 << $1
	DetailPrint "Left Shift: $0 << $1 = $2"
	DetailPrint "There are no Rotates in NSIS"
	
	
	Pop $2
	Pop $1
	Pop $0
FunctionEnd
