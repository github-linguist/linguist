RandGen(MaxBalls){
Random,k,3,MaxBalls
Loop,% k{
	Random,k,1,3
	o.=k
}return o
}
While((!InStr(o,1)||!InStr(o,2)||!InStr(o,3))||!RegExReplace(o,"\b1+2+3+\b"))
	o:=RandGen(3)
Loop,% StrLen(o)
	F.=SubStr(o,A_Index,1) ","
F:=RTrim(F,",")
Sort,F,N D`,
MsgBox,% F:=RegExReplace(RegExReplace(RegExReplace(F,"(1)","Red"),"(2)","White"),"(3)","Blue")
