sub euler_method {
        my ($t0, $t1, $k, $step_size) = @_;
        my @results = ( [0, $t0] );

        for (my $s = $step_size; $s <= 100; $s += $step_size) {
                $t0 -= ($t0 - $t1) * $k * $step_size;
                push @results, [$s, $t0];
        }

        return @results;
}

sub analytical {
        my ($t0, $t1, $k, $time) = @_;
        return ($t0 - $t1) * exp(-$time * $k) + $t1
}

my ($T0, $T1, $k) = (100, 20, .07);
my @r2  = grep { $_->[0] % 10 == 0 } euler_method($T0, $T1, $k, 2);
my @r5  = grep { $_->[0] % 10 == 0 } euler_method($T0, $T1, $k, 5);
my @r10 = grep { $_->[0] % 10 == 0 } euler_method($T0, $T1, $k, 10);

print "Time\t      2     err(%)      5     err(%)    10      err(%)  Analytic\n", "-" x 76, "\n";
for (0 .. $#r2) {
        my $an = analytical($T0, $T1, $k, $r2[$_][0]);
        printf "%4d\t".("%9.3f" x 7)."\n",
                $r2 [$_][0],
                $r2 [$_][1], ($r2 [$_][1] / $an) * 100 - 100,
                $r5 [$_][1], ($r5 [$_][1] / $an) * 100 - 100,
                $r10[$_][1], ($r10[$_][1] / $an) * 100 - 100,
                $an;
}
