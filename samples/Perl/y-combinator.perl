sub Y { my $f = shift;                                # λf.
    sub { my $x = shift; $x->($x) }->(                #   (λx.x x)
	sub {my $y = shift; $f->(sub {$y->($y)(@_)})} #   λy.f λz.y y z
    )
}
my $fac = sub {my $f = shift;
    sub {my $n = shift; $n < 2 ? 1 : $n * $f->($n-1)}
};
my $fib = sub {my $f = shift;
    sub {my $n = shift; $n == 0 ? 0 : $n == 1 ? 1 : $f->($n-1) + $f->($n-2)}
};
for my $f ($fac, $fib) {
    print join(' ', map Y($f)->($_), 0..9), "\n";
}
