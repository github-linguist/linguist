sub combSort {
    my @arr = @_;
    my $gap = @arr;
    my $swaps = 1;
    while ($gap > 1 || $swaps) {
        $gap /= 1.25 if $gap > 1;
        $swaps = 0;
        foreach my $i (0 .. $#arr - $gap) {
            if ($arr[$i] > $arr[$i+$gap]) {
                @arr[$i, $i+$gap] = @arr[$i+$gap, $i];
                $swaps = 1;
            }
        }
    }
    return @arr;
}
