my $number = 1 + int rand 10;
do { print "Guess a number between 1 and 10: " } until <> == $number;
print "You got it!\n";
