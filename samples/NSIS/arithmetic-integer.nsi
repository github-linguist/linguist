Function Arithmetic
	Push $0
	Push $1
	Push $2
	StrCpy $0 21
	StrCpy $1 -2
	
	IntOp $2 $0 + $1
	DetailPrint "$0 + $1 = $2"
	IntOp $2 $0 - $1
	DetailPrint "$0 - $1 = $2"
	IntOp $2 $0 * $1
	DetailPrint "$0 * $1 = $2"
	IntOp $2 $0 / $1
	DetailPrint "$0 / $1 = $2"
	DetailPrint "Rounding is toward negative infinity"
	IntOp $2 $0 % $1
	DetailPrint "$0 % $1 = $2"
	DetailPrint "Sign of remainder matches the first number"
	
	Pop $2
	Pop $1
	Pop $0
FunctionEnd
