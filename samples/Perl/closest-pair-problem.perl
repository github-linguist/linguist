#! /usr/bin/perl
use strict;
use POSIX qw(ceil);

sub dist
{
    my ( $a, $b) = @_;
    return sqrt( ($a->[0] - $b->[0])**2 +
                 ($a->[1] - $b->[1])**2 );
}

sub closest_pair_simple
{
    my $ra = shift;
    my @arr = @$ra;
    my $inf = 1e600;
    return $inf if scalar(@arr) < 2;
    my ( $a, $b, $d ) = ($arr[0], $arr[1], dist($arr[0], $arr[1]));
    while( @arr ) {
	my $p = pop @arr;
	foreach my $l (@arr) {
	    my $t = dist($p, $l);
	    ($a, $b, $d) = ($p, $l, $t) if $t < $d;	
	}
    }
    return ($a, $b, $d);
}

sub closest_pair
{
    my $r = shift;
    my @ax = sort { $a->[0] <=> $b->[0] } @$r;
    my @ay = sort { $a->[1] <=> $b->[1] } @$r;
    return closest_pair_real(\@ax, \@ay);
}

sub closest_pair_real
{
    my ($rx, $ry) = @_;
    my @xP = @$rx;
    my @yP = @$ry;
    my $N = @xP;
    return closest_pair_simple($rx) if scalar(@xP) <= 3;

    my $inf = 1e600;
    my $midx = ceil($N/2)-1;

    my @PL = @xP[0 .. $midx];
    my @PR = @xP[$midx+1 .. $N-1];

    my $xm = ${$xP[$midx]}[0];

    my @yR = ();
    my @yL = ();
    foreach my $p (@yP) {
	if ( ${$p}[0] <= $xm ) {
	    push @yR, $p;
	} else {
	    push @yL, $p;
	}
    }

    my ($al, $bl, $dL) = closest_pair_real(\@PL, \@yR);
    my ($ar, $br, $dR) = closest_pair_real(\@PR, \@yL);

    my ($m1, $m2, $dmin) = ($al, $bl, $dL);
    ($m1, $m2, $dmin) = ($ar, $br, $dR) if $dR < $dL;

    my @yS = ();
    foreach my $p (@yP) {
	push @yS, $p if abs($xm - ${$p}[0]) < $dmin;
    }

    if ( @yS ) {
	my ( $w1, $w2, $closest ) = ($m1, $m2, $dmin);
	foreach my $i (0 .. ($#yS - 1)) {

	    my $k = $i + 1;
	    while ( ($k <= $#yS) && ( (${$yS[$k]}[1] - ${$yS[$i]}[1]) < $dmin) ) {
		my $d = dist($yS[$k], $yS[$i]);
		($w1, $w2, $closest) = ($yS[$k], $yS[$i], $d) if $d < $closest;
		$k++;
	    }

	}
	return ($w1, $w2, $closest);

    } else {
	return ($m1, $m2, $dmin);
    }
}



my @points = ();
my $N = 5000;

foreach my $i (1..$N) {
    push @points, [rand(20)-10.0, rand(20)-10.0];
}


my ($a, $b, $d) = closest_pair_simple(\@points);
print "$d\n";

my ($a1, $b1, $d1) = closest_pair(\@points);
#print "$d1\n";
