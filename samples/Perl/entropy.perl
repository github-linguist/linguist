sub entropy {
    my %count; $count{$_}++ for @_;
    my $entropy = 0;
    for (values %count) {
        my $p = $_/@_;
        $entropy -= $p * log $p;
    }
    $entropy / log 2
}

print entropy split //, "1223334444";
