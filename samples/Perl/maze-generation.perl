use List::Util 'max';

my ($w, $h) = @ARGV;
$w ||= 26;
$h ||= 127;
my $avail = $w * $h;

# cell is padded by sentinel col and row, so I don't check array bounds
my @cell = (map([(('1') x $w), 0], 1 .. $h), [('') x ($w + 1)]);
my @ver = map([("|  ") x $w], 1 .. $h);
my @hor = map([("+--") x $w], 0 .. $h);

sub walk {
	my ($x, $y) = @_;
	$cell[$y][$x] = '';
	$avail-- or return;	# no more bottles, er, cells

	my @d = ([-1, 0], [0, 1], [1, 0], [0, -1]);
	while (@d) {
		my $i = splice @d, int(rand @d), 1;
		my ($x1, $y1) = ($x + $i->[0], $y + $i->[1]);

		$cell[$y1][$x1] or next;

		if ($x == $x1) { $hor[ max($y1, $y) ][$x] = '+  ' }
		if ($y == $y1) { $ver[$y][ max($x1, $x) ] = '   ' }
		walk($x1, $y1);
	}
}

walk(int rand $w, int rand $h);	# generate

for (0 .. $h) {			# display
	print @{$hor[$_]}, "+\n";
	print @{$ver[$_]}, "|\n" if $_ < $h;
}
