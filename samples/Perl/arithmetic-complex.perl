use Math::Complex;
my $a = 1 + 1*i;
my $b = 3.14159 + 1.25*i;

print "$_\n" foreach
    $a + $b,    # addition
    $a * $b,    # multiplication
    -$a,        # negation
    1 / $a,     # multiplicative inverse
    ~$a;        # complex conjugate
