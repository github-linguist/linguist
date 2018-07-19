MsgBox, % GreatCircleDist(36.12, 33.94, -86.67, -118.40, 6372.8, "km")

GreatCircleDist(La1, La2, Lo1, Lo2, R, U) {
	return, 2 * R * ASin(Sqrt(Hs(Rad(La2 - La1)) + Cos(Rad(La1)) * Cos(Rad(La2)) * Hs(Rad(Lo2 - Lo1)))) A_Space U
}

Hs(n) {
	return, (1 - Cos(n)) / 2
}

Rad(Deg) {
	return, Deg * 4 * ATan(1) / 180
}
