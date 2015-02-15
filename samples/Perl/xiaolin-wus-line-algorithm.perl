#!perl
use strict;
use warnings;

sub plot {
	my ($x, $y, $c) = @_;
	printf "plot %d %d %.1f\n", $x, $y, $c if $c;
}

sub ipart {
	int shift;
}

sub round {
	int( 0.5 + shift );
}

sub fpart {
	my $x = shift;
	$x - int $x;
}

sub rfpart {
	1 - fpart(shift);
}

sub drawLine {
	my ($x0, $y0, $x1, $y1) = @_;

	my $plot = \&plot;

	if( abs($y1 - $y0) > abs($x1 - $x0) ) {
		$plot = sub { plot( @_[1, 0, 2] ) };
		($x0, $y0, $x1, $y1) = ($y0, $x0, $y1, $x1);
	}

	if( $x0 > $x1 ) {
		($x0, $x1, $y0, $y1) = ($x1, $x0, $y1, $y0);
	}

	my $dx = $x1 - $x0;
	my $dy = $y1 - $y0;
	my $gradient = $dy / $dx;

	my @xends;
	my $intery;

	# handle the endpoints
	for my $xy ([$x0, $y0], [$x1, $y1]) {
		my ($x, $y) = @$xy;
		my $xend = round($x);
		my $yend = $y + $gradient * ($xend - $x);
		my $xgap = rfpart($x + 0.5);

		my $x_pixel = $xend;
		my $y_pixel = ipart($yend);
		push @xends, $x_pixel;

		$plot->($x_pixel, $y_pixel  , rfpart($yend) * $xgap);
		$plot->($x_pixel, $y_pixel+1,  fpart($yend) * $xgap);
		next if defined $intery;
		# first y-intersection for the main loop
		$intery = $yend + $gradient;
	}

	# main loop

	for my $x ( $xends[0] + 1 .. $xends[1] - 1 ) {
		$plot->($x, ipart ($intery),  rfpart($intery));
		$plot->($x, ipart ($intery)+1, fpart($intery));
		$intery += $gradient;
	}
}

if( $0 eq __FILE__ ) {
	drawLine( 0, 1, 10, 2 );
}
__END__
