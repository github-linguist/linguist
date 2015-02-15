use Math::Complex ':trig';

sub compose {
    my ($f, $g) = @_;

    sub {
        $f -> ($g -> (@_));
    };
}

my $cube  = sub { $_[0] ** (3)   };
my $croot = sub { $_[0] ** (1/3) };

my @flist1 = ( \&Math::Complex::sin, \&Math::Complex::cos, $cube  );
my @flist2 = ( \&asin,               \&acos,               $croot );

print join "\n", map {
    compose($flist1[$_], $flist2[$_]) -> (0.5)
} 0..2;
