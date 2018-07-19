Fib := NStepSequence(1, 2, 2, 20)
Loop, 21 {
	i := A_Index - 1
	, Out .= i ":`t", n := ""
	Loop, % Fib.MaxIndex() {
		x := Fib.MaxIndex() + 1 - A_Index
		if (Fib[x] <= i)
			n .= 1, i -= Fib[x]
		else
			n .= 0
	}
	Out .= (n ? LTrim(n, "0") : 0) "`n"
}
MsgBox, % Out

NStepSequence(v1, v2, n, k) {
    a := [v1, v2]
	Loop, % k - 2 {
		a[j := A_Index + 2] := 0
		Loop, % j < n + 2 ? j - 1 : n
			a[j] += a[j - A_Index]
	}
	return, a
}
