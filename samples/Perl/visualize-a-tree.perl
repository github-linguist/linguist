#!/usr/bin/perl
use warnings;
use strict;
use utf8;
use open OUT => ':utf8', ':std';

sub parse {
    my ($tree) = shift;
    if (my ($root, $children) = $tree =~ /^(.+?)\((.*)\)$/) {

        my $depth = 0;
        for my $pos (0 .. length($children) - 1) {
            my $char = \substr $children, $pos, 1;
            if (0 == $depth and ',' eq $$char) {
                $$char = "\x0";
            } elsif ('(' eq $$char) {
                $depth++;
            } elsif (')' eq $$char) {
                $depth--;
            }
        }
        return($root, [map parse($_), split /\x0/, $children]);

    } else { # Leaf.
        return $tree;
    }
}

sub output {
    my ($parsed, $prefix) = @_;
    my $is_root = not defined $prefix;
    $prefix //= ' ';
    while (my $member = shift @$parsed) {
        my $last = !@$parsed || (1 == @$parsed and ref $parsed->[0]);
        unless ($is_root) {
            substr $prefix, -3, 1, ' ';
            substr($prefix, -4, 1) =~ s/├/│/;
            substr $prefix, -2, 1, ref $member ? ' ' : '└' if $last;
        }

        if (ref $member) {
            output($member, $prefix . '├─');
        } else {
            print $prefix, $member, "\n";
        }
    }
}

my $tree = 'a(b0(c1,c2(d(ef,gh)),c3(i1,i2,i3(jj),i4(kk,m))),b1(C1,C2(D1(E),D2,D3),C3))';
my $parsed = [parse($tree)];
output($parsed);
