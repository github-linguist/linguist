use warnings;
use strict;

my $initial = join ",", qw(abc def ghi);
my %reverse = qw(X O O X);

# In list context, returns best move,
# In scalar context, returns the score of best move.
my %cache;
sub best_move {
	my ($b, $me) = @_;
	if( exists $cache{$b,$me,wantarray} ) {
		return $cache{$b,$me,wantarray};
	} elsif( my $s = score( $b, $me ) ) {
		return $cache{$b,$me,wantarray} = (wantarray ? undef : $s);
	}
	my $him = $reverse{$me};
	my ($best, @best) = (-999);
	for my $m (moves($b)) {
		(my $with_m = $b) =~ s/$m/$me/ or die;
		# The || operator supplies scalar context to best_move(...)
		my $s = -(score($with_m, $him) || best_move($with_m, $him));
		if( $s > $best ) {
			($best, @best) = ($s, $m);
		} elsif( $s == $best ) {
			push @best, $m;
		}
	}
	$cache{$b,$me,wantarray} = wantarray ? $best[rand @best] : $best;
}

my $winner = q[([XOxo])(?:\1\1|...\1...\1|..\1..\1|....\1....\1)];
sub score {
	my ($b, $me) = @_;
	$b =~ m/$winner/o or return 0;
	return $1 eq $me ? +1 : -1;
}

sub moves {
	my ($b) = @_;
	$b =~ /([^xoXO,\n])/g;
}

sub print_board {
	my ($b) = @_;
	$b =~ s/\B/|/g;
	$b =~ s/,/\n-+-+-\n/g;
	print $b, "\n";
}

sub prompt {
	my ($b, $color) = @_;
	my @moves = moves($b);
	unless( @moves ) {
		return;
	}
	while( 1 ) {
		print "Place your $color on one of [@moves]: ";
		defined(my $m = <>) or return;
		chomp($m);
		return $m if grep $m eq $_, @moves;
	}
}

my @players = (
	{ whose => "your", name => "You",
	  verb => "You place", get_move => \&prompt },
	{ whose => "the computer's", name => "Computer",
	  verb => "The computer places", get_move => \&best_move },
);
my $whose_turn = int rand 2;

my $color = "X";
my $b = $initial;

while( 1 ) {
	my $p = $players[$whose_turn];
	print_board($b);
	print "It is $p->{whose} turn.\n";
	# The parens around $m supply list context to the right side
	# or the = operator, which causes sub best_move to return the
	# best move, rather than the score of the best move.
	my ( $m ) = $p->{get_move}->($b, $color);
	if( $m ) {
		print "$p->{verb} an $color at $m\n";
		$b =~ s/$m/$color/;
		my $s = score($b, $color) or next;
		print_board($b);
		print "$p->{name} ", $s > 0 ? "won!\n" : "lost!\n";
	} else {
		print "$p->{name} cannot move.\n";
	}
	print "Game over.\nNew Game...\n";
	($b, $color, $whose_turn) = ($initial, "X", int rand 2);
	redo;
} continue {
	$color = $reverse{$color};
	$whose_turn = !$whose_turn;
}
