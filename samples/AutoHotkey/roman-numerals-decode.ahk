Roman_Decode(str){
	res := 0
	Loop Parse, str
	{
		n := {M: 1000, D:500, C:100, L:50, X:10, V:5, I:1}[A_LoopField]
		If ( n > OldN ) && OldN
			res -= 2*OldN
		res += n, oldN := n
	}
	return res
}

test = MCMXC|MMVIII|MDCLXVI
Loop Parse, test, |
   res .= A_LoopField "`t= " Roman_Decode(A_LoopField) "`r`n"
clipboard := res
