use List::Util 'min';

sub ham_gen {
	my @s = ([1], [1], [1]);
	my @m = (2, 3, 5);

	return sub {
	#	use bigint;
		my $n = min($s[0][0], $s[1][0], $s[2][0]);
		for (0 .. 2) {
			shift @{$s[$_]} if $s[$_][0] == $n;
			push @{$s[$_]}, $n * $m[$_]
		}

		return $n
	}
}

my ($h, $i) = ham_gen;

++$i, print $h->(), " " until $i > 20;
print "...\n";

++$i, $h->() until $i == 1690;
print ++$i, "-th: ", $h->(), "\n";
