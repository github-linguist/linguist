gray_encode(n){
	return n ^ (n >> 1)
}

gray_decode(n){
	p := n
	while (n >>= 1)
		p ^= n
	return p
}

BinString(n){
	Loop 5
		If ( n & ( 1 << (A_Index-1) ) )
			o := "1" . o
		else	o := "0" . o
	return o
}

Loop 32
	n:=A_Index-1, out .= n " : " BinString(n) " => " BinString(e:=gray_encode(n))
			. " => " BinString(gray_decode(e)) " => " BinString(n) "`n"
MsgBox % clipboard := out
