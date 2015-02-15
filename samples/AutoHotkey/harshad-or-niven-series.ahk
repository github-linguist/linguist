H := []
n := 1

Loop
	n := (H[A_Index] := NextHarshad(n)) + 1
until  H[H.MaxIndex()] > 1000

Loop, 20
	Out .= H[A_Index] ", "

MsgBox, % Out ". . . " H[H.MaxIndex()]

NextHarshad(n) {
	Loop, {
		Loop, Parse, n
			sum += A_LoopField
		if (!Mod(n, sum))
			return n
		n++, sum := ""
	}
}
