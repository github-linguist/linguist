sub gcd {
    my ($n, $m) = @_;
    while($n){
        my $t = $n;
        $n = $m % $n;
        $m = $t;
    }
    return $m;
}

sub tripel {
    my $pmax  = shift;
    my $prim  = 0;
    my $count = 0;
    my $nmax = sqrt($pmax)/2;
    for( my $n=1; $n<=$nmax; $n++ ) {
        for( my $m=$n+1; (my $p = 2*$m*($m+$n)) <= $pmax; $m+=2 ) {
            next unless 1==gcd($m,$n);
            $prim++;
            $count += int $pmax/$p;
        }
    }
    printf "Max. perimeter: %d, Total: %d, Primitive: %d\n", $pmax, $count, $prim;
}

tripel 10**$_ for 1..8;
