sub accumulator {
  my $sum = shift;
  sub { $sum += shift }
}

my $x = accumulator(1);
$x->(5);
print accumulator(3), "\n";
print $x->(2.3), "\n";
