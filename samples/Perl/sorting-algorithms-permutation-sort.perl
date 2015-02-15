sub psort {
        my ($x, $d) = @_;

        unless ($d //= $#$x) {
                $x->[$_] < $x->[$_ - 1] and return for 1 .. $#$x;
                return 1
        }

        for (0 .. $d) {
                unshift @$x, splice @$x, $d, 1;
                next if $x->[$d] < $x->[$d - 1];
                return 1 if psort($x, $d - 1);
        }
}

my @a = map+(int rand 100), 0 .. 10;
print "Before:\t@a\n";
psort(\@a);
print "After:\t@a\n"
