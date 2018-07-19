sub A {
    my ($k, $x1, $x2, $x3, $x4, $x5) = @_;
    my($B);
    $B = sub { A(--$k, $B, $x1, $x2, $x3, $x4) };
    $k <= 0 ? &$x4 + &$x5 : &$B;
}

print A(10, sub{1}, sub {-1}, sub{-1}, sub{1}, sub{0} ), "\n";
