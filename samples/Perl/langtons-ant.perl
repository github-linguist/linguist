#!/usr/bin/perl
use strict;
# Perl 5 implementation of Langton's Ant

# Using screen coordinates - 0,0 in upper-left, +X right, +Y down -
# these directions (right, up, left, down) are counterclockwise
# so advance through the array to turn left, retreat to turn right
my @dirs = ( [1,0], [0,-1], [-1,0], [0,1] );
my $size = 100;

# we treat any false as white and true as black, so undef is fine for initial all-white grid
my @plane;
for (0..$size-1) { $plane[$_] = [] };

# start out in approximate middle
my ($x, $y) = ($size/2, $size/2);

# pointing in a random direction
my $dir = int rand @dirs;

my $move;
for ($move = 0; $x >= 0 && $x < $size && $y >= 0 && $y < $size; $move++) {
  # toggle cell's value (white->black or black->white)
  if ($plane[$x][$y] = 1 - ($plane[$x][$y] ||= 0)) {
        # if it's now true (black), then it was white, so turn right
        $dir = ($dir - 1) % @dirs;
  } else {
        # otherwise it was black, so turn left
        $dir = ($dir + 1) % @dirs;
  }
  $x += $dirs[$dir][0];
  $y += $dirs[$dir][1];
}

print "Out of bounds after $move moves at ($x, $y)\n";
for (my $y=0; $y<$size; ++$y) {
  for (my $x=0; $x<$size; ++$x) {
    print $plane[$x][$y] ? '#' : '.';
  }
  print "\n";
}
