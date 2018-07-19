use strict;

my ($w, $h) = (9, 9);
my @v = map([ (0) x ($w + 1) ], 0 .. $h); # voltage
my @f = map([ (0) x ($w + 1) ], 0 .. $h); # fixed condition
my @d = map([ (0) x ($w + 1) ], 0 .. $h); # diff

my @n; # neighbors
for my $i (0 .. $h) {
	push @{$n[$i][$_]}, [$i, $_ - 1] for 1 .. $w;
	push @{$n[$i][$_]}, [$i, $_ + 1] for 0 .. $w - 1;
}
for my $j (0 .. $w) {
	push @{$n[$_][$j]}, [$_ - 1, $j] for 1 .. $h;
	push @{$n[$_][$j]}, [$_ + 1, $j] for 0 .. $h - 1;
}

sub set_boundary {
	$f[1][1] = 1; $f[6][7] = -1;
	$v[1][1] = 1; $v[6][7] = -1;
}

sub calc_diff {
	my $total_diff;
	for my $i (0 .. $h) {
		for my $j (0 .. $w) {
			my ($p, $v) = $n[$i][$j];
			$v += $v[$_->[0]][$_->[1]] for @$p;
			$d[$i][$j] = $v = $v[$i][$j] - $v / scalar(@$p);
			$total_diff += $v * $v unless $f[$i][$j];
		}
	}
	$total_diff;
}

sub iter {
	my $diff = 1;
	while ($diff > 1e-24) { # 1e-24 is overkill (12 digits of precision)
		set_boundary();
		$diff = calc_diff();
		print "error^2: $diff\r";
		for my $i (0 .. $h) {
			for my $j (0 .. $w) {
				$v[$i][$j] -= $d[$i][$j];
			}
		}
	}
	print "\n";

	my @current = (0) x 3;
	for my $i (0 .. $h) {
		for my $j (0 .. $w) {
			$current[ $f[$i][$j] ] +=
				$d[$i][$j] * scalar(@{$n[$i][$j]});
		}
	}
	return ($current[1] - $current[-1]) / 2;
}

print "R = @{[2 / iter()]}\n";
