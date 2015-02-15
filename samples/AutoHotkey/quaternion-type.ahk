q  := [1, 2, 3, 4]
q1 := [2, 3, 4, 5]
q2 := [3, 4, 5, 6]
r := 7

MsgBox, % "q = " PrintQ(q)
	. "`nq1 = " PrintQ(q1)
	. "`nq2 = " PrintQ(q2)
	. "`nr = " r
	. "`nNorm(q) = " Norm(q)
	. "`nNegative(q) = " PrintQ(Negative(q))
	. "`nConjugate(q) = " PrintQ(Conjugate(q))
	. "`nq + r = " PrintQ(AddR(q, r))
	. "`nq1 + q2 = " PrintQ(AddQ(q1, q2))
	. "`nq2 + q1 = " PrintQ(AddQ(q2, q1))
	. "`nqr = " PrintQ(MulR(q, r))
	. "`nq1q2 = " PrintQ(MulQ(q1, q2))
	. "`nq2q1 = " PrintQ(MulQ(q2, q1))

Norm(q) {
	return sqrt(q[1]**2 + q[2]**2 + q[3]**2 + q[4]**2)
}

Negative(q) {
	a := []
	for k, v in q
		a[A_Index] := v * -1
	return a
}

Conjugate(q) {
	a := []
	for k, v in q
		a[A_Index] := v * (A_Index = 1 ? 1 : -1)
	return a
}

AddR(q, r) {
	a := []
	for k, v in q
		a[A_Index] := v + (A_Index = 1 ? r : 0)
	return a
}

AddQ(q1, q2) {
	a := []
	for k, v in q1
		a[A_Index] := v + q2[A_Index]
	return a
}

MulR(q, r) {
	a := []
	for k, v in q
		a[A_Index] := v * r
	return a
}

MulQ(q, u) {
	a := []
	, a[1] := q[1]*u[1] - q[2]*u[2] - q[3]*u[3] - q[4]*u[4]
	, a[2] := q[1]*u[2] + q[2]*u[1] + q[3]*u[4] - q[4]*u[3]
	, a[3] := q[1]*u[3] - q[2]*u[4] + q[3]*u[1] + q[4]*u[2]
	, a[4] := q[1]*u[4] + q[2]*u[3] - q[3]*u[2] + q[4]*u[1]
	return a
}

PrintQ(q, b="(") {
	for k, v in q
		b .= v (A_Index = q.MaxIndex() ? ")" : ", ")
	return b
}
