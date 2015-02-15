#!perl
use strict;
use warnings qw(FATAL all);
my @initial = split /\n/, <<'';
#############
#  #        #
# $$$$$$$  @#
#.......    #
#############

#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######

=for
space is an empty square
# is a wall
@ is the player
$ is a box
. is a goal
+ is the player on a goal
* is a box on a goal
=cut


my $cols = length($initial[0]);
my $initial = join '', @initial;
my $size = length($initial);
die unless $size == $cols * @initial;

sub WALL() { 1 }
sub PLAYER() { 2 }
sub BOX() { 4 }
sub GOAL() { 8 }

my %input = (
	' ' => 0, '#' => WALL, '@' => PLAYER, '$' => BOX,
	'.' => GOAL, '+' => PLAYER|GOAL, '*' => BOX|GOAL,
);
my %output = reverse(%input);

sub packed_initial {
	my $ret = '';
	vec( $ret, $_, 4 ) = $input{substr $initial, $_, 1}
		for( 0 .. $size-1 );
	$ret;
}

sub printable_board {
	my $board = shift;
	my @c = @output{map vec($board, $_, 4), 0 .. $size-1};
	my $ret = '';
	while( my @row = splice @c, 0, $cols ) {
		$ret .= join '', @row, "\n";
	}
	$ret;
}

my $packed = packed_initial();

my @udlr = qw(u d l r);
my @UDLR = qw(U D L R);
my @deltas = (-$cols, +$cols, -1, +1);

my %fseen;
INIT_FORWARD: {
	$initial =~ /(\@|\+)/ or die;
	use vars qw(@ftodo @fnext);
	@ftodo = (["", $packed,  $-[0]]);
	$fseen{$packed} = '';
}

my %rseen;
INIT_REVERSE: {
	my $goal = $packed;
	vec($goal, $ftodo[0][2], 4) -= PLAYER;
	my @u = grep { my $t = vec($goal, $_, 4); $t & GOAL and not $t & BOX } 0 .. $size-1;
	my @b = grep { my $t = vec($goal, $_, 4); $t & BOX and not $t & GOAL } 0 .. $size-1;
	die unless @u == @b;
	vec($goal, $_, 4) += BOX for @u;
	vec($goal, $_, 4) -= BOX for @b;
	use vars qw(@rtodo @rnext);
	FINAL_PLACE: for my $player (0 .. $size-1) {
		next if vec($goal, $player, 4);
		FIND_GOAL: {
			vec($goal, $player + $_, 4) & GOAL and last FIND_GOAL for @deltas;
			next FINAL_PLACE;
		}
		my $a_goal = $goal;
		vec($a_goal, $player, 4) += PLAYER;
		push @rtodo, ["", $a_goal, $player ];
		$rseen{$a_goal} = '';
		#print printable_board($a_goal);
	}
}

my $movelen = -1;
my ($solution);
MAIN: while( @ftodo and @rtodo ) {

	FORWARD: {
		my ($moves, $level, $player) = @{pop @ftodo};
		die unless vec($level, $player, 4) & PLAYER;

		for my $dir_num (0 .. 3) {
			my $delta = $deltas[$dir_num];
			my @loc = map $player + $delta * $_, 0 .. 2;
			my @val = map vec($level, $_, 4), @loc;

			next if $val[1] & WALL or ($val[1] & BOX and $val[2] & (BOX|WALL));
			
			my $new = $level;
			vec($new, $loc[0], 4) -= PLAYER;
			vec($new, $loc[1], 4) += PLAYER;
			my $nmoves;
			if( $val[1] & BOX ) {
				vec($new, $loc[1], 4) -= BOX;
				vec($new, $loc[2], 4) += BOX;
				$nmoves = $moves . $UDLR[$dir_num];
			} else {
				$nmoves = $moves . $udlr[$dir_num];
			}
			
			next if exists $fseen{$new};
			$fseen{$new} = $nmoves;

			push @fnext, [ $nmoves, $new, $loc[1] ];

			exists $rseen{$new} or next;
			#print(($val[1] & BOX) ? "Push $UDLR[$dir_num]\n" : "Fwalk $udlr[$dir_num]\n");
			$solution = $new;
			last MAIN;
		}

		last FORWARD if @ftodo;
		use vars qw(*ftodo *fnext);
		(*ftodo, *fnext) = (\@fnext, \@ftodo);
	} # end FORWARD
	
	BACKWARD: {
		my ($moves, $level, $player) = @{pop @rtodo};
		die "<$level>" unless vec($level, $player, 4) & PLAYER;

		for my $dir_num (0 .. 3) {
			my $delta = $deltas[$dir_num];
			# look behind and in front of the player.
			my @loc = map $player + $delta * $_, -1 .. 1;
			my @val = map vec($level, $_, 4), @loc;

			# unlike the forward solution, we cannot push boxes
			next if $val[0] & (WALL|BOX);
			my $new = $level;
			vec($new, $loc[0], 4) += PLAYER;
			vec($new, $loc[1], 4) -= PLAYER;
			# unlike the forward solution, if we have a box behind us
			# we can *either* pull it or not.  This means there are
			# two "successors" to this board.
			if( $val[2] & BOX ) {
				my $pull = $new;
				vec($pull, $loc[2], 4) -= BOX;
				vec($pull, $loc[1], 4) += BOX;
				goto RWALK if exists $rseen{$pull};
				my $pmoves = $UDLR[$dir_num] . $moves;
				$rseen{$pull} = $pmoves;
				push @rnext, [$pmoves, $pull, $loc[0]];
				goto RWALK unless exists $fseen{$pull};
				print "Doing pull\n";
				$solution = $pull;
				last MAIN;
			}
			RWALK:
			next if exists $rseen{$new}; # next direction.
			my $wmoves = $udlr[$dir_num] . $moves;
			$rseen{$new} = $wmoves;
			push @rnext, [$wmoves, $new, $loc[0]];
			next unless exists $fseen{$new};
			print "Rwalk\n";
			$solution = $new;
			last MAIN;
		}

		last BACKWARD if @rtodo;
		use vars qw(*rtodo *rnext);
		(*rtodo, *rnext) = (\@rnext, \@rtodo);
	} # end BACKWARD
}

if( $solution ) {
	my $fmoves = $fseen{$solution};
	my $rmoves = $rseen{$solution};
	print "Solution found!\n";
	print "Time: ", (time() - $^T), " seconds\n";
	print "Moves: $fmoves $rmoves\n";
	print "Move Length: ", length($fmoves . $rmoves), "\n";
	print "Middle Board: \n", printable_board($solution);
} else {
	print "No solution found!\n";
}
__END__
