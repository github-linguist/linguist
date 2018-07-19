MsgBox, % "The mean time is: " MeanTime(["23:00:17", "23:40:20", "00:12:45", "00:17:19"])

MeanTime(t, x=0, y=0) {
	c := ATan(1) / 45
	for k, v in t
		n := StrSplit(v, ":")
		, r := c * (n[1] * 3600 + n[2] * 60 + n[3]) / 240
		, x += Cos(r)
		, y += Sin(r)
	r := atan2(x, y) / c
	r := (r < 0 ? r + 360 : r) / 15
	h := SubStr("00" Round(r // 1, 0), -1)
	s := SubStr("00" Round(Mod(m := Mod(r, 1) * 60, 1) * 60, 0), -1)
	m := SubStr("00" Round(m // 1, 0), -1)
	return, h ":" m ":" s
}

atan2(x, y) {
   return dllcall("msvcrt\atan2", "Double",y, "Double",x, "CDECL Double")
}
