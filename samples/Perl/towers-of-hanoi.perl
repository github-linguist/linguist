sub hanoi {
    my ($n, $from, $to, $via) = (@_, 1, 2, 3);

    if ($n == 1) {
        print "Move disk from pole $from to pole $to.\n";
    } else {
        hanoi($n - 1, $from, $via, $to);
        hanoi(1, $from, $to, $via);
        hanoi($n - 1, $via, $to, $from);
    };
};
