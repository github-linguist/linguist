local fn ReverseBits(n) {
	local rev = 0
	while (n > 0) {
		rev = rev << 1
		if (n & 1) == 1 {
			rev = rev ^^ 1
		}
		n = n >> 1
	}
	return rev
}

print(ReverseBits(11))