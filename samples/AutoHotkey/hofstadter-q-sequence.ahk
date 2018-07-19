SetBatchLines, -1
Q := HofsQSeq(100000)

Loop, 10
	Out .= Q[A_Index] ", "

MsgBox, % "First ten:`t" Out "`n"
	. "1000th:`t`t" Q[1000] "`n"
	. "Flips:`t`t" Q.flips

HofsQSeq(n) {
	Q := {1: 1, 2: 1, "flips": 0}
	Loop, % n - 2 {
		i := A_Index + 2
		,	Q[i] := Q[i - Q[i - 1]] + Q[i - Q[A_Index]]
		if (Q[i] < Q[i - 1])
			Q.flips++
	}
	return Q
}
