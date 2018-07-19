mapRange(a1, a2, b1, b2, s)
{
	return b1 + (s-a1)*(b2-b1)/(a2-a1)
}

out := "Mapping [0,10] to [-1,0] at intervals of 1:`n"

Loop 11
	out .= "f(" A_Index-1 ") = " mapRange(0,10,-1,0,A_Index-1) "`n"
MsgBox % out
