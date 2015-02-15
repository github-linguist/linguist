#!/usr/bin/perl -w
use strict;

my $out = 0;
my $max_out = -1;
my @max_times;

open FH, '<mlijobs.txt' or die "Can't open file: $!";
while (<FH>) {
    chomp;
    if (/OUT/) {
        $out++;
    } else {
        $out--;
    }
    if ($out > $max_out) {
        $max_out = $out;
        @max_times = ();
    }
    if ($out == $max_out) {
        push @max_times, (split)[3];
    }
}
close FH;

print "Maximum simultaneous license use is $max_out at the following times:\n";
print "  $_\n" foreach @max_times;
