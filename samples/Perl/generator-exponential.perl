# gen_pow($m) creates and returns an anonymous subroutine that will
# generate and return the powers 0**m, 1**m, 2**m, ...
sub gen_pow {
    my $m = shift;
    my $e = 0;
    return sub { return $e++ ** $m; };
}

# gen_filter($g1, $g2) generates everything returned from $g1 that
# is not also returned from $g2. Both $g1 and $g2 must be references
# to subroutines that generate numbers in increasing order. gen_filter
# creates and returns an anonymous subroutine.
sub gen_filter {
    my($g1, $g2) = @_;
    my $v1;
    my $v2 = $g2->();
    return sub {
        for (;;) {
            $v1 = $g1->();
            $v2 = $g2->() while $v1 > $v2;
            return $v1 unless $v1 == $v2;
        }
    };
}

# Create generators.
my $squares = gen_pow(2);
my $cubes = gen_pow(3);
my $squares_without_cubes = gen_filter($squares, $cubes);

# Drop 20 values.
$squares_without_cubes->() for (1..20);

# Print 10 values.
my @answer;
push @answer, $squares_without_cubes->() for (1..10);
print "[", join(", ", @answer), "]\n";
