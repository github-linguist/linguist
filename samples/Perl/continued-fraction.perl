sub continued_fraction {
    my ($a, $b, $n) = (@_[0,1], $_[2] // 100);

    $a->() + ($n && $b->() / continued_fraction($a, $b, $n-1));
}

printf "√2  ≈ %.9f\n", continued_fraction do { my $n; sub { $n++ ? 2 : 1 } }, sub { 1 };
printf "e   ≈ %.9f\n", continued_fraction do { my $n; sub { $n++ || 2 } }, do { my $n; sub { $n++ || 1 } };
printf "π   ≈ %.9f\n", continued_fraction do { my $n; sub { $n++ ? 6 : 3 } }, do { my $n; sub { (2*$n++ + 1)**2 } }, 1_000;
printf "π/2 ≈ %.9f\n", continued_fraction do { my $n; sub { 1/($n++ || 1) } }, sub { 1 }, 1_000;
