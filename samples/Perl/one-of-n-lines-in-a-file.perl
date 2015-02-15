#!/usr/bin/perl
use warnings;
use strict;

sub one_of_n {
    my $n = shift;
    my $return = 1;
    for my $line (2 .. $n) {
        $return = $line if 1 > rand $line;
    }
    return $return;
}

my $repeat = 1_000_000;
my $size   = 10;

my @freq;
++$freq[ one_of_n($size) - 1 ] for 1 .. $repeat;
print "@freq\n";
