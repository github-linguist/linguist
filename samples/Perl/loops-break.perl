while (1) {
    my $a = int(rand(20));
    print "$a\n";
    if ($a == 10) {
        last;
    }
    my $b = int(rand(20));
    print "$b\n";
}
