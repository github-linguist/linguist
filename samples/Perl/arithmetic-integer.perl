my $a = <>;
my $b = <>;

print
    "sum:              ", $a + $b, "\n",
    "difference:       ", $a - $b, "\n",
    "product:          ", $a * $b, "\n",
    "integer quotient: ", int($a / $b), "\n",
    "remainder:        ", $a % $b, "\n",
    "exponent:         ", $a ** $b, "\n"
    ;
