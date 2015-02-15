#!/usr/bin/perl

my $min = 1;
my $max = 99;
my $guess = int(rand $max) + $min;
my $tries = 0;

print "=>> Think of a number between $min and $max and I'll guess it!\n
Press <ENTER> when are you ready... ";

<STDIN>;

{
    do {

        $tries++;
        print "\n=>> My guess is: $guess Is your number higher, lower, or equal? (h/l/e)\n> ";

        my $score = <STDIN>;

        if ($max <= $min) {
            print "\nI give up...\n" and last;
        } elsif ($score =~ /^h/i) {
            $min = $guess + 1;
        } elsif ($score =~ /^l/i) {
            $max = $guess;
        } elsif ($score =~ /^e/i) {
            print "\nI knew it! It took me only $tries tries.\n" and last;
        } else {
            print "error: invalid score\n";
        }

        $guess = int(($max + $min) / 2);

    } while(1);
}
