sub dotprod
{
        my($vec_a, $vec_b) = @_;
        die "they must have the same size\n" unless @$vec_a == @$vec_b;
        my $sum = 0;
        $sum += $vec_a->[$_] * $vec_b->[$_] for 0..$#$vec_a;
        return $sum;
}

my @vec_a = (1,3,-5);
my @vec_b = (4,-2,-1);

print dotprod(\@vec_a,\@vec_b), "\n"; # 3
