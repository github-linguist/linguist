use Test::Base;
use Test::More;

__DATA__
=== Strict Test

--- perl strict
my $x = 5;
--- strict
use strict;
use warnings;
my $x = 5;