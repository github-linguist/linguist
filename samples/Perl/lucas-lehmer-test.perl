sub is_prime {
    my $p = shift;
    if ($p == 2) {
        return 1;
    } elsif ($p <= 1 || $p % 2 == 0) {
        return 0;
    } else {
        my $limit = sqrt($p);
        for (my $i = 3; $i <= $limit; $i += 2) {
            return 0 if $p % $i == 0;
        }
        return 1;
    }
}

sub is_mersenne_prime {
    use bigint;
    my $p = shift;
    if ($p == 2) {
        return 1;
    } else {
        my $m_p = 2 ** $p - 1;
        my $s = 4;

        foreach my $i (3 .. $p) {
            $s = ($s ** 2 - 2) % $m_p;
        }
        return $s == 0;
    }
}

my $precision = 20000;   # maximum requested number of decimal places of 2 ** MP-1 #
my $long_bits_width = $precision / log(2) * log(10);
my $upb_prime = int(($long_bits_width - 1)/2);    # no unsigned #
my $upb_count = 45;      # find 45 mprimes if int was given enough bits #

print " Finding Mersenne primes in M[2..$upb_prime]:\n";

my $count = 0;
foreach my $p (2 .. $upb_prime) {
    if (is_prime($p) && is_mersenne_prime($p)) {
        print "M$p\n";
        $count++;
    }
    last if $count >= $upb_count;
}
