#!/usr/bin/perl
use integer;
use strict;

my @A = qw(
    5 3 0  0 2 4  7 0 0
    0 0 2  0 0 0  8 0 0
    1 0 0  7 0 3  9 0 2

    0 0 8  0 7 2  0 4 9
    0 2 0  9 8 0  0 7 0
    7 9 0  0 0 0  0 8 0

    0 0 0  0 3 0  5 0 6
    9 6 0  0 1 0  3 0 0
    0 5 0  6 9 0  0 1 0
);

sub solve {
    my $i;
    foreach $i ( 0 .. 80 ) {
	next if $A[$i];
	my %t = map {
		$_ / 9 == $i / 9 ||
		$_ % 9 == $i % 9 ||
		$_ / 27 == $i / 27 && $_ % 9 / 3 == $i % 9 / 3
		? $A[$_] : 0,
		1;
	    } 0 .. 80;
	solve( $A[$i] = $_ ) for grep !$t{$_}, 1 .. 9;
	return $A[$i] = 0;
    }
    $i = 0;
    foreach (@A) {
	print "-----+-----+-----\n" if !($i%27) && $i;
	print !($i%9) ? '': $i%3 ? ' ' : '|', $_;
	print "\n" unless ++$i%9;
    }
}
solve();
