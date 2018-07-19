use Data::Random qw(rand_set);
use List::MoreUtils qw(uniq);

my $size = 4;
my $chosen = join "", rand_set set => ["1".."9"], size => $size;

print "I've chosen a number from $size unique digits from 1 to 9; you need
to input $size unique digits to guess my number\n";

for ( my $guesses = 1; ; $guesses++ ) {
    my $guess;
    while (1) {
        print "\nNext guess [$guesses]: ";
        $guess = <STDIN>;
        chomp $guess;
        checkguess($guess) and last;
        print "$size digits, no repetition, no 0... retry\n";
    }
    if ( $guess eq $chosen ) {
        print "You did it in $guesses attempts!\n";
        last;
    }
    my $bulls = 0;
    my $cows = 0;
    for my $i (0 .. $size-1) {
        if ( substr($guess, $i, 1) eq substr($chosen, $i, 1) ) {
            $bulls++;
        } elsif ( index($chosen, substr($guess, $i, 1)) >= 0 ) {
            $cows++;
        }
    }
    print "$cows cows, $bulls bulls\n";
}

sub checkguess
{
    my $g = shift;
    return uniq(split //, $g) == $size && $g =~ /^[1-9]{$size}$/;
}
