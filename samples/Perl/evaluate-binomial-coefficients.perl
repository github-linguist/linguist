sub binomial {
	use bigint;
	my ($r, $n, $k) = (1, @_);
	for (1 .. $k) {	$r *= $n + 1 - $_; $r /= $_ }
	$r;
}

print binomial(30, 13);
