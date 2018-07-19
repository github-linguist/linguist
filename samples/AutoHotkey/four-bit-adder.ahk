A := 13
B := 9
N := FourBitAdd(A, B)
MsgBox, % A " + " B ":`n"
	. GetBin4(A) " + " GetBin4(B) " = " N.S " (Carry = " N.C ")"
return

Xor(A, B) {
	return (~A & B) | (A & ~B)
}

HalfAdd(A, B) {
	return {"S": Xor(A, B), "C": A & B}
}

FullAdd(A, B, C=0) {
	X := HalfAdd(A, C)
	Y := HalfAdd(B, X.S)
	return {"S": Y.S, "C": X.C | Y.C}
}

FourBitAdd(A, B, C=0) {
	A := GetFourBits(A)
	B := GetFourBits(B)
	X := FullAdd(A[4], B[4], C)
	Y := FullAdd(A[3], B[3], X.C)
	W := FullAdd(A[2], B[2], Y.C)
	Z := FullAdd(A[1], B[1], W.C)
	return {"S": Z.S W.S Y.S X.S, "C": Z.C}
}

GetFourBits(N) {
	if (N < 0 || N > 15)
		return -1
	return StrSplit(GetBin4(N))
}

GetBin4(N) {
	Loop 4
		Res := Mod(N, 2) Res, N := N >> 1
	return, Res
}
