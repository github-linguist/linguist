#!/usr/bin/perl
use warnings;
use strict;
use feature qw(say);

sub fangs {
    my $vampire = shift;
    my $length  = length 0 + $vampire;
    return if $length % 2;
    my $fang_length = $length / 2;
    my $from        = '1' . '0' x ($fang_length - 1);
    my $to          = '9' x $fang_length;
    my $sorted      = join q(), sort split //, $vampire;
    my @fangs;
    for my $f1 ($from .. 1 + sqrt $vampire) {
        next if $vampire % $f1;
        my $f2 = $vampire / $f1;
        next if $sorted ne join q(), sort split //, $f1 . $f2;
        next if 2 == grep '0' eq substr($_, -1 , 1), $f1, $f2; # Needed for the 26th number.
        push @fangs, [$f1, $f2];
    }
    return @fangs;
}

my $count = 0;
my $i     = 9;
while ($count < 25) {
    $i++;
    my @f = fangs($i);
    $count++, say join ' ', "$count. $i:", map "[@$_]", @f if @f;
}

say join ' ', $_, map "[@$_]", fangs($_) for 16758243290880, 24959017348650, 14593825548650;
