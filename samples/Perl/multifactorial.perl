{ # <-- scoping the cache and bigint clause
	my @cache;
	use bigint;
	sub mfact {
		my ($s, $n) = @_;
		return 1 if $n <= 0;
		$cache[$s][$n] //= $n * mfact($s, $n - $s);
	}
}

for my $s (1 .. 10) {
	print "step=$s: ";
	print join(" ", map(mfact($s, $_), 1 .. 10)), "\n";
}
