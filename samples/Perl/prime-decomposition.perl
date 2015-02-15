sub prime_factors {
	my ($n, $d, @out) = (shift, 1);
	while ($n > 1 && $d++) {
		$n /= $d, push @out, $d until $n % $d;
	}
	@out
}

print "@{[prime_factors(1001)]}\n";
