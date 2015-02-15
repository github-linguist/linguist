use Math::Complex;

sub mandelbrot {
    my ($z, $c) = @_[0,0];
    for (1 .. 20) {
        $z = $z * $z + $c;
        return $_ if abs $z > 2;
    }
}

for (my $y = 1; $y >= -1; $y -= 0.05) {
    for (my $x = -2; $x <= 0.5; $x += 0.0315)
        {print mandelbrot($x + $y * i) ? ' ' : '#'}
    print "\n"
}
