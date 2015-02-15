sub A
{
        my $a = 0;
        $a += $_ for @_;
        return $a / @_;
}
sub G
{
        my $p = 1;
        $p *= $_ for @_;
        return  $p**(1/@_); # power of 1/n == root of n
}
sub H
{
        my $h = 0;
        $h += 1/$_ for @_;
        return @_/$h;
}
my @ints = (1..10);

my $a = A(@ints);
my $g = G(@ints);
my $h = H(@ints);

print "A=$a\nG=$g\nH=$h\n";
die "Error" unless $a >= $g and $g >= $h;
