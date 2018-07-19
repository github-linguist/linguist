#!/usr/bin/perl

use strict;
use warnings;

use utf8;

sub deal {
    my $s = shift;

    my $rnd = sub {
        return (($s = ($s * 214013 + 2531011) & 0x7fffffff) >> 16 );
    };

    my @d;
    for my $b (split "", "A23456789TJQK") {
        push @d, map("$_$b", qw/♣ ♦ ♥ ♠/);
    }

    for my $idx (reverse 0 .. $#d) {
        my $r = $rnd->() % ($idx + 1);
        @d[$r, $idx] = @d[$idx, $r];
    }

    return [reverse @d];
}

my $hand_idx = shift(@ARGV) // 11_982;

my $cards = deal($hand_idx);

my $num_cards_in_height = 8;
my $string = '';

while (@$cards)
{
    $string .= join(' ', splice(@$cards, 0, 8)) . "\n";
}

binmode STDOUT, ':encoding(utf-8)';
print "Hand $hand_idx\n";
print $string;
