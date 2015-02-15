factorize(n){
	if n = 1
		return 1
	if n < 1
		return false
	result := 0, m := n, k := 2
	While n >= k{
		while !Mod(m, k){
			result .= " * " . k, m /= k
		}
		k++
	}
	return SubStr(result, 5)
}
Loop 22
   out .= A_Index ": " factorize(A_index) "`n"
MsgBox % out
