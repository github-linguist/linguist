my @items = sort { $b->[2]/$b->[1] <=> $a->[2]/$a->[1] }
(
        [qw'beef    3.8 36'],
        [qw'pork    5.4 43'],
        [qw'ham     3.6 90'],
        [qw'greaves 2.4 45'],
        [qw'flitch  4.0 30'],
        [qw'brawn   2.5 56'],
        [qw'welt    3.7 67'],
        [qw'salami  3.0 95'],
        [qw'sausage 5.9 98'],
);

my ($limit, $value) = (15, 0);

print "item   fraction weight value\n";
for (@items) {
        my $ratio = $_->[1] > $limit ? $limit/$_->[1] : 1;
        print "$_->[0]\t";
        $value += $_->[2] * $ratio;
        $limit -= $_->[1];
        if ($ratio == 1) {
                print "  all\t$_->[1]\t$_->[2]\n";
        } else {
                printf "%5.3f   %s   %8.3f\n", $ratio, $_->[1] * $ratio, $_->[2] * $ratio;
                last;
        }
}

print "-" x 40, "\ntotal value: $value\n";
