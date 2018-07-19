use bigrat;

foreach my $candidate (2 .. 2**19) {
    my $sum = 1 / $candidate;
    foreach my $factor (2 .. sqrt($candidate)+1) {
        if ($candidate % $factor == 0) {
            $sum += 1 / $factor + 1 / ($candidate / $factor);
        }
    }
    if ($sum->denominator() == 1) {
        print "Sum of recipr. factors of $candidate = $sum exactly ", ($sum == 1 ? "perfect!" : ""), "\n";
    }
}
