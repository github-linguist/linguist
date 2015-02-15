use strict;

sub sq {
	my $s = 0;
	$s += $_ ** 2 for @_;
	$s;
}

sub hit {
	my ($sph, $x, $y) = @_;
	$x -= $sph->[0];
	$y -= $sph->[1];

	my $z = sq($sph->[3]) - sq($x, $y);
	return	if $z < 0;

	$z = sqrt $z;
	return $sph->[2] - $z, $sph->[2] + $z;
}

sub normalize {
	my $v = shift;
	my $n = sqrt sq(@$v);
	$_ /= $n for @$v;
	$v;
}

sub dot {
	my ($x, $y) = @_;
	my $s = $x->[0] * $y->[0] + $x->[1] * $y->[1] + $x->[2] * $y->[2];
	$s > 0 ? $s : 0;
}

my $pos = [ 120, 120, 0, 120 ];
my $neg = [ -77, -33, -100, 190 ];
my $light = normalize([ -12, 13, -10 ]);
sub draw {
	my ($k, $amb) = @_;
	binmode STDOUT, ":raw";
	print "P5\n", $pos->[0] * 2 + 3, " ", $pos->[1] * 2 + 3, "\n255\n";
	for my $y (($pos->[1] - $pos->[3] - 1) .. ($pos->[1] + $pos->[3] + 1)) {
		my @row = ();
		for my $x (($pos->[0] - $pos->[3] - 1) .. ($pos->[0] + $pos->[3] + 1)) {
			my ($hit, @hs) = 0;
			my @h = hit($pos, $x, $y);

			if (!@h) { $hit = 0 }
			elsif (!(@hs = hit($neg, $x, $y))) { $hit = 1 }
			elsif ($hs[0] > $h[0]) { $hit = 1 }
			elsif ($hs[1] > $h[0]) { $hit = $hs[1] > $h[1] ? 0 : 2 }
			else { $hit = 1 }

			my ($val, $v);
			if ($hit == 0) { $val = 0 }
			elsif ($hit == 1) {
				$v = [	$x - $pos->[0],
					$y - $pos->[1],
					$h[0] - $pos->[2] ];
			} else {
				$v = [	$neg->[0] - $x,
					$neg->[1] - $y,
					$neg->[2] - $hs[1] ];
			}
			if ($v) {
				normalize($v);
				$val = int((dot($v, $light) ** $k + $amb) * 255);
				$val = ($val > 255) ? 255 : ($val < 0) ? 0 : $val;
			}
			push @row, $val;
		}
		print pack("C*", @row);
	}
}

draw(2, 0.2);
