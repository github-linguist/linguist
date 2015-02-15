#NoEnv
SetBatchLines, -1
#SingleInstance, Force

; Greatest common divisor, from http://rosettacode.org/wiki/Greatest_common_divisor#AutoHotkey
gcd(a,b) {
	Return b=0 ? Abs(a) : Gcd(b,mod(a,b))
}

count_triples(max) {
	primitives := 0, triples := 0, m := 2
	while m <= (max / 2)**0.5
	{
		n := mod(m, 2) + 1
		,p := 2*m*(m + n)
		, delta := 4*m
		while n < m and p <= max
			gcd(m, n) = 1
				? (primitives++
				, triples += max // p)
				: ""
			, n += 2
			, p += delta
		m++
	}
	Return primitives " primitives out of " triples " triples"
}

Loop, 8
	Msgbox % 10**A_Index ": " count_triples(10**A_Index)
