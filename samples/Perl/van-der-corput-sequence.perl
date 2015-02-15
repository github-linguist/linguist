sub vdc {
    my @value = shift;
    my $base = shift // 2;
    use integer;
    push @value, $value[-1] / $base while $value[-1] > 0;
    my ($x, $sum) = (1, 0);
    no integer;
    $sum += ($_ % $base) / ($x *= $base) for @value;
    return $sum;
}

for my $base ( 2 .. 5 ) {
    print "base $base: ", join ' ', map { vdc($_, $base) } 0 .. 10;
    print "\n";
}
