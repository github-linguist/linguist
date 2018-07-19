SetBatchLines, -1
MsgBox, % "Largest left-truncatable and right-truncatable primes less than one million:`n"
	. "Left:`t" LTP(10 ** 6) "`nRight:`t" RTP(10 ** 6)

LTP(n) {
	while n {
		n--
		if (!Instr(n, "0") && IsPrime(n)) {
			Loop, % StrLen(n)
				if (!IsPrime(SubStr(n, A_Index)))
					continue, 2
			break
		}
	}
	return, n
}

RTP(n) {
	while n {
		n--
		if (!IsPrime(SubStr(n, 1, 1)))
			n -= 10 ** (StrLen(n) - 1)
		if (!Instr(n, "0") && IsPrime(n)) {
			Loop, % StrLen(n)
				if (!IsPrime(SubStr(n, 1, A_Index)))
					continue, 2
			break
		}
	}
	return, n
}

IsPrime(n) {
	if (n < 2)
		return, 0
	else if (n < 4)
		return, 1
	else if (!Mod(n, 2))
		return, 0
	else if (n < 9)
		return 1
	else if (!Mod(n, 3))
		return, 0
	else {
		r := Floor(Sqrt(n))
		f := 5
		while (f <= r) {
			if (!Mod(n, f))
				return, 0
			if (!Mod(n, (f + 2)))
				return, 0
			f += 6
		}
		return, 1
	}
}
