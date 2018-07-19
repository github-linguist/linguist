#!perl
use strict;
use warnings;

# This code was adapted from the perl6 solution for this task.

# Each element of the deck is an integer, which, when written
# in octal, has four digits, which are all either 1, 2, or 4.

my $fmt = '%4o';
my @deck = grep sprintf($fmt, $_) !~ tr/124//c, 01111 .. 04444;

# Given a feature digit (1, 2, or 4), produce the feature's name.
# Note that digits 0 and 3 are unused.
my @features = map [split ' '], split /\n/,<<'';
! red   green    ! purple
! one   two      ! three
! oval  squiggle ! diamond
! solid open     ! striped

81 == @deck or die "There are ".@deck." cards (should be 81)";

# By default, draw 9 cards, but if the user
# supplied a parameter, use that.
my $draw = shift(@ARGV) || 9;
my $goal = int($draw/2);

# Get the possible combinations of 3 indices into $draw elements.
my @combinations = combine(3, 0 .. $draw-1);

my @sets;

do {
	# Shuffle the first $draw elements of @deck.
	for my $i ( 0 .. $draw-1 ) {
		my $j = $i + int rand(@deck - $i);
		@deck[$i, $j] = @deck[$j, $i];
	}

	# Find all valid sets using the shuffled elements.
	@sets = grep {
		my $or = 0;
		$or |= $_ for @deck[@$_];
		# If all colors (or whatever) are the same, then
		# a 1, 2, or 4 will result when we OR them together.
		# If they're all different, then a 7 will result.
		# If any other digit occurs, the set is invalid.
		sprintf($fmt, $or) !~ tr/1247//c;
	} @combinations;

	# Continue until there are exactly $goal valid sets.
} until @sets == $goal;

print "Drew $draw cards:\n";
for my $i ( 0 .. $#sets ) {
	print "Set ", $i+1, ":\n";
	my @cards = @deck[ @{$sets[$i]} ];
	for my $card ( @cards ) {
		my @octal = split //, sprintf '%4o', $card;
		my @f = map $features[$_][$octal[$_]], 0 .. 3;
		printf "    %-6s %-5s %-8s %s\n", @f;
	}
}

exit;

# This function is adapted from the perl5i solution for the
# RosettaCode Combinations task.
sub combine {
	my $n = shift;
	return unless @_;
	return map [$_], @_ if $n == 1;
	my $head = shift;
	my @result = combine( $n-1, @_ );
	unshift @$_, $head for @result;
	@result, combine( $n, @_ );
}

__END__
