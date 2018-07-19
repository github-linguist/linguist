if 1 = ; no parameter passed
{
	InputBox, 1, Last Fridays of year, Enter a year:, , , , , , , , %A_YYYY%
	If ErrorLevel
		ExitApp
}

YYYY = %1% ; retrieve command line parameter
Stmp = %YYYY%0101000000
count= 0

While count < 12
{
	FormatTime, ddd, %stmp%, ddd
	FormatTime, M, %stmp%, M
	If (ddd = "Fri"){
		if (M-1 = count){
			t := stmp
			stmp += 7, days
		}
		else
			 res .= SubStr(t, 1, 4) "-" SubStr(t, 5, 2) "-" SubStr(t, 7, 2) "`n"
			,count++
			,stmp := YYYY . SubStr("0" M, -1) . "01"
	}
	else
		stmp += 1, days
}
MsgBox % res
