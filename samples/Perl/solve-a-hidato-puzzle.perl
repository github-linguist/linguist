use strict;
use List::Util 'max';

our (@grid, @known, $n);

sub show_board {
	for my $r (@grid) {
		print map(!defined($_)	? '   ' : $_
					? sprintf("%3d", $_)
					: ' __'
			, @$r), "\n"
	}
}

sub parse_board {
	@grid = map{[map(/^_/ ? 0 : /^\./ ? undef: $_, split ' ')]}
			split "\n", shift();
	for my $y (0 .. $#grid) {
		for my $x (0 .. $#{$grid[$y]}) {
			$grid[$y][$x] > 0
				and $known[$grid[$y][$x]] = "$y,$x";
		}
	}
	$n = max(map { max @$_ } @grid);
}

sub neighbors {
	my ($y, $x) = @_;
	my @out;
	for (	[-1, -1], [-1, 0], [-1, 1],
		[ 0, -1],	   [ 0, 1],
		[ 1, -1], [ 1, 0], [ 1, 1])
	{
		my $y1 = $y + $_->[0];
		my $x1 = $x + $_->[1];
		next if $x1 < 0 || $y1 < 0;
		next unless defined $grid[$y1][$x1];
		push @out, "$y1,$x1";
	}
	@out
}

sub try_fill {
	my ($v, $coord) = @_;
	return 1	if $v > $n;

	my ($y, $x) = split ',', $coord;
	my $old = $grid[$y][$x];

	return	if $old && $old != $v;
	return	if exists $known[$v] and $known[$v] ne $coord;

	$grid[$y][$x] = $v;
	print "\033[0H";
	show_board();

	try_fill($v + 1, $_) && return 1
			for neighbors($y, $x);

	$grid[$y][$x] = $old;
	return
}

parse_board
#	". 4 .
#	 _ 7 _
#	 1 _ _";

#	" 1 _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . 74
#	  . . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _ . _
#	  . . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _ . . _ _
#	";

	"__ 33 35 __ __ .. .. .. .
	 __ __ 24 22 __ .. .. .. .
	 __ __ __ 21 __ __ .. .. .
	 __ 26 __ 13 40 11 .. .. .
	 27 __ __ __  9 __  1 .. .
	 .   . __ __ 18 __ __ .. .
	 .  ..  .  . __  7 __ __ .
	 .  .. .. ..  .  .  5 __ .";

print "\033[2J";
try_fill(1, $known[1]);
