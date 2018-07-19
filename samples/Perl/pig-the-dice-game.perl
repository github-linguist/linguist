#!perl
use strict;
use warnings;
my @players = @ARGV;
@players = qw(Joe Mike);
my @scores = (0) x @players;
while( 1 ) {
	PLAYER: for my $i ( 0 .. $#players ) {
		my $name = $players[$i];
		my $score = $scores[$i];
		my $roundscore = 1 + int rand 6;
		print "$name, your score so far is $score.\n";
		print "You rolled a $roundscore.\n";
		next PLAYER if $roundscore == 1;
		while($score + $roundscore < 100) {
			print "Roll again, or hold [r/h]: ";
			my $answer = <>;
			$answer = 'h' unless defined $answer;
			if( $answer =~ /^h/i ) {
				$score += $roundscore;
				$scores[$i] = $score;
				print "Your score is now $score.\n";
				next PLAYER;
			} elsif( $answer =~ /^r/ ) {
				my $die = 1 + int rand 6;
				print "$name, you rolled a $die.\n";
				next PLAYER if $die == 1;
				$roundscore += $die;
				print "Your score for the round is now $roundscore.\n";
			} else {
				print "I did not understand that.\n";
			}
		}
		$score += $roundscore;
		print "With that, your score became $score.\n";
		print "You won!\n";
		exit;
	}
}
__END__
