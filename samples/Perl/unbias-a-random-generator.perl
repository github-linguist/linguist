sub randn {
        my $n = shift;
        return int(rand($n) / ($n - 1));
}

for my $n (3 .. 6) {
        print "Bias $n: ";
        my (@raw, @fixed);
        for (1 .. 10000) {
                my $x = randn($n);
                $raw[$x]++;
                $fixed[$x]++    if randn($n) != $x
        }
        print "@raw, ";
        printf("%3g+-%.3g%%\tfixed: ", $raw[0]/100,
		100 * sqrt($raw[0] * $raw[1]) / ($raw[0] + $raw[1])**1.5);
        print "@fixed, ";
        printf("%3g+-%.3g%%\n", 100*$fixed[0]/($fixed[0] + $fixed[1]),
		100 * sqrt($fixed[0] * $fixed[1]) / ($fixed[0] + $fixed[1])**1.5);

}
