#!/usr/bin/perl
use warnings;
use strict;

use constant {
    LEFT  => 0,
    RIGHT => 1,
};

{   my @primes = (2, 3);

    sub is_prime {
        my $n = shift;
        return if $n < 2;

        for my $prime (@primes) {
            last if $prime >= $n;
            return unless $n % $prime;
        }

        my $sqrt = sqrt $n;
        while ($primes[-1] < $sqrt) {
            my $new = 2 + $primes[-1];
            $new += 2 until is_prime($new);
            push @primes, $new;
            return unless $n % $new;
        }

        return 1;
    }
}


sub trunc {
    my ($n, $side) = @_;
    substr $n, $side == LEFT ? 0 : -1, 1, q();
    return $n;
}


sub is_tprime { # Absence of zeroes is tested outside the sub.
    my ($n, $side) = @_;
    return (is_prime($n)
            and (1 == length $n or is_tprime(trunc($n, $side), $side)));
}


my $length = 6;
my @tprimes = ('9' x $length) x 2;
for my $side (LEFT, RIGHT) {
    $tprimes[$side] -= 2 until -1 == index $tprimes[$side], '0'
                               and is_tprime($tprimes[$side], $side);
}

print 'left ', join(', right ', @tprimes), "\n";
