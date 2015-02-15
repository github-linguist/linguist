use strict;
use warnings;
# Find a knight's tour

my @board;

# Choose starting position - may be passed in on command line; if
# not, choose random square.
my ($i, $j);
if (my $sq = shift @ARGV) {
  die "$0: illegal start square '$sq'\n" unless ($i, $j) = from_algebraic($sq);
} else {
  ($i, $j) = (int rand 8, int rand 8);
}

# Move sequence
my @moves = ();

foreach my $move (1..64) {
  # Record current move
  push @moves, to_algebraic($i,$j);
  $board[$i][$j] = $move;

  # Get list of possible next moves
  my @targets = possible_moves($i,$j);

  # Find the one with the smallest degree
  my @min = (9);
  foreach my $target (@targets) {
      my ($ni, $nj) = @$target;
      my $next = possible_moves($ni,$nj);
      @min = ($next, $ni, $nj) if $next < $min[0];
  }

  # And make it
  ($i, $j) = @min[1,2];
}

# Print the move list
for (my $i=0; $i<4; ++$i) {
  for (my $j=0; $j<16; ++$j) {
    my $n = $i*16+$j;
    print $moves[$n];
    print ', ' unless $n+1 >= @moves;
  }
  print "\n";
}
print "\n";

# And the board, with move numbers
for (my $i=0; $i<8; ++$i) {
  for (my $j=0; $j<8; ++$j) {
    # Assumes (1) ANSI sequences work, and (2) output
    # is light text on a dark background.
    print "\e[7m" if ($i%2==$j%2);
    printf " %2d", $board[$i][$j];
    print "\e[0m";
  }
  print "\n";
}

# Find the list of positions the knight can move to from the given square
sub possible_moves
{
  my ($i, $j) = @_;
  return grep { $_->[0] >= 0 && $_->[0] < 8
                    && $_->[1] >= 0 && $_->[1] < 8
                    && !$board[$_->[0]][$_->[1]] } (
                    [$i-2,$j-1], [$i-2,$j+1], [$i-1,$j-2], [$i-1,$j+2],
                    [$i+1,$j-2], [$i+1,$j+2], [$i+2,$j-1], [$i+2,$j+1]);
}

# Return the algebraic name of the square identified by the coordinates
# i=rank, 0=black's home row; j=file, 0=white's queen's rook
sub to_algebraic
{
   my ($i, $j) = @_;
   chr(ord('a') + $j) . (8-$i);
}

# Return the coordinates matching the given algebraic name
sub from_algebraic
{
   my $square = shift;
   return unless $square =~ /^([a-h])([1-8])$/;
   return (8-$2, ord($1) - ord('a'));
}
