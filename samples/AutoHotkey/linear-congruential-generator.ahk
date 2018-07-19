a := 0, b:= [0]
Loop, 10
	BSD .= "`t" (a :=  BSD(a)) "`n"
,	b := MS(b[1])
,	MS .= "`t" (b[2]) "`n"

MsgBox, % "BSD:`n" BSD "`nMS:`n" MS

BSD(Seed) {
	return, Mod(1103515245 * Seed + 12345, 2147483648)
}

MS(Seed) {
	Seed := Mod(214013 * Seed + 2531011, 2147483648)
	return, [Seed, Seed // 65536]
}
