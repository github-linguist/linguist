S := [], M := []

FileRead, dict, unixdict.txt
Loop, Parse, dict, `n, `r`n
{
	r := Reverse(A_LoopField)
	if (S[r])
		M.Insert(r " / " A_LoopField)
	else
		S[A_LoopField] := 1
}

Loop, 5
	Out .= "`t" M[A_Index] "`n"

MsgBox, % "5 Examples:`n" Out "`nTotal Pairs:`n`t" M.MaxIndex()

Reverse(s) {
	Loop, Parse, s
		r := A_LoopField . r
	return r
}
