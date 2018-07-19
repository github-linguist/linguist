my $PI = 2 * atan2 1, 0;

my @nums = map {
    1 + 0.5 * sqrt(-2 * log rand) * cos(2 * $PI * rand)
} 1..1000;
