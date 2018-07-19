sub sma_generator {
    my $period = shift;
    my (@list, $sum);

    return sub {
        my $number = shift;
        push @list, $number;
        $sum += $number;
        $sum -= shift @list if @list > $period;
        return $sum / @list;
    }
}

# Usage:
my $sma = sma_generator(3);
for (1, 2, 3, 2, 7) {
    printf "append $_ --> sma = %.2f  (with period 3)\n", $sma->($_);
}
