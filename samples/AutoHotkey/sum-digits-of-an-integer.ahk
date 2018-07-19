MsgBox % sprintf("%d %d %d %d %d`n"
	,SumDigits(1, 10)
	,SumDigits(12345, 10)
	,SumDigits(123045, 10)
	,SumDigits(0xfe, 16)
	,SumDigits(0xf0e, 16) )

SumDigits(n,base) {
	sum := 0
	while (n)
	{
		sum += Mod(n,base)
		n /= base
	}
	return sum
}

sprintf(s,fmt*) {
	for each, f in fmt
		StringReplace,s,s,`%d, % f
	return s
}
