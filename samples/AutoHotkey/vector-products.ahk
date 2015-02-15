V := {a: [3, 4, 5], b: [4, 3, 5], c: [-5, -12, -13]}

for key, val in V
	Out .= key " = (" val[1] ", " val[2] ", " val[3] ")`n"

CP := CrossProduct(V.a, V.b)
VTP := VectorTripleProduct(V.a, V.b, V.c)

MsgBox, % Out "`na • b = " DotProduct(V.a, V.b) "`n"
	. "a x b = (" CP[1] ", " CP[2] ", " CP[3] ")`n"
	. "a • b x c = " ScalerTripleProduct(V.a, V.b, V.c) "`n"
	. "a x b x c = (" VTP[1] ", " VTP[2] ", " VTP[3] ")"

DotProduct(v1, v2) {
	return, v1[1] * v2[1] + v1[2] * v2[2] + v1[3] * v2[3]
}

CrossProduct(v1, v2) {
	return, [v1[2] * v2[3] - v1[3] * v2[2]
	      ,  v1[3] * v2[1] - v1[1] * v2[3]
	      ,  v1[1] * v2[2] - v1[2] * v2[1]]
}

ScalerTripleProduct(v1, v2, v3) {
	return, DotProduct(v1, CrossProduct(v2, v3))
}

VectorTripleProduct(v1, v2, v3) {
	return, CrossProduct(v1, CrossProduct(v2, v3))
}
