# Task 2 from
# https://perlweeklychallenge.org/blog/perl-weekly-challenge-075/

my @hist = 3, 2, 3, 5, 7, 5;

my $max = 0;
my @max;

for ^@hist -> $start {
    for $start ^..^ @hist -> $end {
        my $area = min(@hist[$start .. $end]) * (1 + $end - $start);
        # say "$start..$end = $area";
        if $area > $max {
            $max = $area;
            @max = $start, $end;
        }
    }
} 

say "The biggest rectangle is between the columns @max[0] and @max[1] and its area is $max.";

# Output:
# $ raku ch-2.raku 
# 0..1 = 4
# 0..2 = 6
# 0..3 = 8
# 0..4 = 10
# 0..5 = 12
# 1..2 = 4
# 1..3 = 6
# 1..4 = 8
# 1..5 = 10
# 2..3 = 6
# 2..4 = 9
# 2..5 = 12
# 3..4 = 10
# 3..5 = 15
# 4..5 = 10
# The biggest rectangle is between the columns 3 and 5 and its area is 15.