Numbers:="123,12345,1234567,987654321,10001,-10001,-123,-100,100,-12345,1,2,-1,-10,2002,-2002,0"
Loop, parse, Numbers, `,
{
	if A_LoopField is not number
		log := log . A_LoopField . "`t: Not a valid number`n"
	else if((d:=StrLen(n:=RegExReplace(A_LoopField,"\D")))<3)
		log := log . A_LoopField . "`t: Too short`n"
	else if(!Mod(d,2))
		log := log . A_LoopField . "`t: Not an odd number of digits`n"
	else
		log := log . A_LoopField . "`t: " . SubStr(n,((d-3)//2)+1,3) . "`n"
}
MsgBox % log
