MsgBox, % ModInv(42, 2017)

ModInv(a, b) {
	if (b = 1)
		return 1
	b0 := b, x0 := 0, x1 :=1
	while (a > 1) {
		q := a // b
		, t  := b
		, b  := Mod(a, b)
		, a  := t
		, t  := x0
		, x0 := x1 - q * x0
		, x1 := t
	}
	if (x1 < 0)
		x1 += b0
	return x1
}
