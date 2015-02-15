Angles :=  [[350, 10], [90, 180, 270, 360], [10, 20, 30]]
MsgBox, % MeanAngle(Angles[1]) "`n"
	. MeanAngle(Angles[2]) "`n"
	. MeanAngle(Angles[3])

MeanAngle(a, x=0, y=0) {
	c := ATan(1) / 45
	for k, v in a
		x += Cos(v * c) / a.MaxIndex()
	,	y += Sin(v * c) / a.MaxIndex()
	return atan2(x, y) / c
}

atan2(x, y) {
   return dllcall("msvcrt\atan2", "Double",y, "Double",x, "CDECL Double")
}
