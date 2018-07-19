use 5.10.0;    # for given/when
sub merge {
        my ($x, $y) = @_;
        my @out;
        while (@$x and @$y) {
                given ($x->[-1] <=> $y->[-1]) {
                        when( 1) { unshift @out, pop @$x }
                        when(-1) { unshift @out, pop @$y }
                        default  { splice @out, 0, 0, pop(@$x), pop(@$y) }
                }
        }
        return @$x, @$y, @out
}

sub strand {
        my $x = shift;
        my @out = shift @$x // return;
        if (@$x) {
                for (-@$x .. -1) {
                        if ($x->[$_] >= $out[-1]) {
                                push @out, splice @$x, $_, 1
                        }
                }
        }
        return @out
}

sub strand_sort {
        my @x = @_;
        my @out;
        while (my @strand = strand(\@x)) {
                @out = merge(\@out, \@strand)
        }
        @out
}

my @a = map (int rand(100), 1 .. 10);
say "Before @a";
@a = strand_sort(@a);
say "After  @a";
