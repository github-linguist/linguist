r := InitR(292929)

Loop, 10
	Out .= (A_Index + 219) ":`t" GetRand(r) "`n"

MsgBox, % Out

GetRand(r) {
	i := Mod(r["j"], 55)
	, r[i] := Mod(r[i] - r[Mod(i + 31, 55)], r["m"])
	, r["j"] += 1
	return, (r[i] < 0 ? r[i] + r["m"] : r[i])
}

InitR(Seed) {
	r := {"j": 0, "m": 10 ** 9}, s := {0: Seed, 1: 1}
	Loop, 53
		s[A_Index + 1] := Mod(s[A_Index - 1] - s[A_Index], r["m"])
	Loop, 55
		r[A_Index - 1] := s[Mod(34 * A_Index, 55)]
	Loop, 165
		i := Mod(A_Index + 54, 55)
		, r[i] := Mod(r[i] - r[Mod(A_Index + 30, 55)], r["m"])
	return, r
}
