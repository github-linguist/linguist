my $a = [ map [ map { int(rand(20)) + 1 } 1 .. 10 ], 1 .. 10];

Outer:
foreach (@$a) {
    foreach (@$_) {
        print " $_";
        if ($_ == 20) {
            last Outer;
        }
    }
    print "\n";
}
print "\n";
