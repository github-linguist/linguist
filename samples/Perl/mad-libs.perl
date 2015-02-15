#!/usr/bin/perl
use warnings;
use strict;

my $template = shift;
open my $IN, '<', $template or die $!;
my $story = do { local $/ ; <$IN> };

my %blanks;
undef $blanks{$_} for $story =~ m/<(.*?)>/g;

for my $blank (sort keys %blanks) {
    print "$blank: ";
    chomp (my $replacement = <>);
    $blanks{$blank} = $replacement;
}

$story =~ s/<(.*?)>/$blanks{$1}/g;
print $story;
