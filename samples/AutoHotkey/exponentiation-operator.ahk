MsgBox % Pow(5,3)
MsgBox % Pow(2.5,4)

Pow(x, n){
	r:=1
	loop %n%
		r *= x
	return r
}
