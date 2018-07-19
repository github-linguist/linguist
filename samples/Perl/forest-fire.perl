use 5.10.0;

my $w = `tput cols` - 1;
my $h = `tput lines` - 1;
my $r = "\033[H";

my ($green, $red, $yellow, $norm) = ("\033[32m", "\033[31m", "\033[33m", "\033[m");

my $tree_prob = .05;
my $burn_prob = .0002;

my @forest = map([ map((rand(1) < $tree_prob) ? 1 : 0, 1 .. $w) ], 1 .. $h);

sub iterate {
	my @new = map([ map(0, 1 .. $w) ], 1 .. $h);
	for my $i (0 .. $h - 1) {
	for my $j (0 .. $w - 1) {
		$new[$i][$j] = $forest[$i][$j];
		if ($forest[$i][$j] == 2) {
			$new[$i][$j] = 3;
			next;
		} elsif ($forest[$i][$j] == 1) {
			if (rand() < $burn_prob) {
				$new[$i][$j] = 2;
				next;
			}
			for (	[-1, -1], [-1, 0], [-1, 1],
				[ 0, -1], 	   [ 0, 1],
				[ 1, -1], [ 1, 0], [ 1, 1] )
			{
				my $y = $_->[0] + $i;
				next if $y < 0 || $y >= $h;
				my $x = $_->[1] + $j;
				next if $x < 0 || $x >= $w;
				if ($forest[$y][$x] == 2) {
					$new[$i][$j] = 2;
					last;
				}
			}
		} elsif (rand() < $tree_prob) {
			$new[$i][$j] = 1;
		} elsif ($forest[$i][$j] == 3) {
			$new[$i][$j] = 0;
		}
	}}
	@forest = @new;
}

sub forest {
	print $r;
	for (@forest) {
		for (@$_) {
			when(0) { print " "; }
			when(1) { print "${green}*"}
			when(2) { print "${red}&" }
			when(3) { print "${yellow}&" }
		}
		print "\033[E\033[1G";
	}
	iterate;
}

forest while (1);
