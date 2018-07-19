#!/usr/bin/perl
use warnings;
use strict;

use List::Util qw{ sum };


sub means {
    my @groups = @_;
    return map sum(@$_) / @$_, @groups;
}


sub following {
    my $pattern    = shift;
    my $orig_count = grep $_, @$pattern;
    my $count;
    do {
        my $i = $#{$pattern};
        until (0 > $i) {
            $pattern->[$i] = $pattern->[$i] ? 0 : 1;
            last if $pattern->[$i];
            --$i;
        }
        $count = grep $_, @$pattern;
    } until $count == $orig_count or not $count;
    undef @$pattern unless $count;
}


my @groups;
my $i = 0;
while (<DATA>) {
    chomp;
    $i++, next if /^$/;
    push @{ $groups[$i] }, $_;
}

my @orig_means = means(@groups);
my $orig_cmp   = $orig_means[0] - $orig_means[1];

my $pattern = [ (0) x @{ $groups[0] },
                (1) x @{ $groups[1] }
              ];

my @cmp = (0) x 3;
while (@$pattern) {
    my @perms = map { my $g = $_;
                      [ (@{ $groups[0] }, @{ $groups[1] } ) [ grep $pattern->[$_] == $g, 0 .. $#{$pattern} ] ];
                  } 0, 1;
    my @means = means(@perms);
    $cmp[ ($means[0] - $means[1]) <=> $orig_cmp ]++;
} continue {
    following($pattern);
}
my $all    = sum(@cmp);
my $length = length $all;
for (0, -1, 1) {
    printf "%-7s %${length}d %6.3f%%\n",
        (qw(equal greater less))[$_], $cmp[$_], 100 * $cmp[$_] / $all;
}


__DATA__
85
88
75
66
25
29
83
39
97

68
41
10
49
16
65
32
92
28
98
