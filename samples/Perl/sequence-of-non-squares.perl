sub nonsqr { my $n = shift; $n + int(0.5 + sqrt($n)) }
print join(' ', map nonsqr($_), 1..22), "\n";
foreach my $i (1..1_000_000) {
  my $j = sqrt(nonsqr($i));
  $j != int($j) or die "Found a square in the sequence: $i";
}
