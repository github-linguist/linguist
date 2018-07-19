#!/usr/bin/perl
use strict;
use warnings;

open(FH, "<", "unixdict.txt") or die "Can't open file!\n";
my @words;
while (<FH>) {
   chomp;
   push @{$words[length]}, $_ if $_ eq join("", sort split(//));
}
close FH;
print "@{$words[-1]}\n";
