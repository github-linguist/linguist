sub shuffle {
  my @a = @_;
  foreach my $n (1 .. $#a) {
    my $k = int rand $n + 1;
    $k == $n or @a[$k, $n] = @a[$n, $k];
  }
  return @a;
}
