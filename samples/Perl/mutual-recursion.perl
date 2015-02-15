sub F { my $n = shift; $n ? $n - M(F($n-1)) : 1 }
sub M { my $n = shift; $n ? $n - F(M($n-1)) : 0 }

# Usage:
foreach my $sequence (\&F, \&M) {
    print join(' ', map $sequence->($_), 0 .. 19), "\n";
}
