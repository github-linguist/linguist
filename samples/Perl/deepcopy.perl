#!/usr/bin/perl
use strict;
use warnings;
use Storable;
use Data::Dumper;

my $src = { foo => 0, bar => [0, 1] };
$src->{baz} = $src;
my $dst = Storable::dclone($src);
print Dumper($src);
print Dumper($dst);
