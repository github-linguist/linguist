sub bin2gray
{
    return $_[0] ^ ($_[0] >> 1);
}

sub gray2bin
{
    my ($num)= @_;
    my $bin= $num;
    while( $num >>= 1 ) {
        # a bit ends up flipped iff an odd number of bits to its left is set.
        $bin ^= $num;   # different from the suggested algorithm;
    }                   # avoids using bit mask and explicit bittery
    return $bin;
}

for (0..31) {
    my $gr= bin2gray($_);
    printf "%d\t%b\t%b\t%b\n", $_, $_, $gr, gray2bin($gr);
}
