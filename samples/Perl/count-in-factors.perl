use utf8;
sub factors {
	my $n = shift;
	my $p = 2;
	my @out;

	while ($n >= $p * $p) {
		while ($n % $p == 0) {
			push @out, $p;
			$n /= $p;
		}
		$p = next_prime($p);
	}
	push @out, $n if $n > 1 || !@out;
	@out;
}

sub next_prime {
	my $p = shift;
	do { $p = $p == 2 ? 3 : $p + 2 } until is_prime($p);
	$p;
}

sub is_prime { factors(shift) == 1 }

print "$_ = ", join(" × ", factors($_)), "\n" for (1 .. 1000);
