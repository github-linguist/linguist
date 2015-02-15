MsgBox % Ethiopian(17, 34) "`n" Ethiopian2(17, 34)

; func definitions:
half( x ) {
	return x >> 1
}

double( x ) {
	return x << 1
}

isEven( x ) {
	return x & 1 == 0
}

Ethiopian( a, b ) {
	r := 0
	While (a >= 1) {
		if !isEven(a)
			r += b
		a := half(a)
		b := double(b)
	}
	return r
}

; or a recursive function:
Ethiopian2( a, b, r = 0 ) { ;omit r param on initial call
	return a==1 ? r+b : Ethiopian2( half(a), double(b), !isEven(a) ? r+b : r )
}
