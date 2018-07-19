>>> def luhn(n):
	r = [int(ch) for ch in str(n)][::-1]
	return (sum(r[0::2]) + sum(sum(divmod(d*2,10)) for d in r[1::2])) % 10 == 0

>>> for n in (49927398716, 49927398717, 1234567812345678, 1234567812345670):
	print(n, luhn(n))
