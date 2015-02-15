sub dif {
  my @s = @_;
  map { $s[$_+1] - $s[$_] } 0 .. $#s-1
}

sub difn {
  my ($n, @s) = @_;
  @s = dif @s foreach 1..$n;
  @s
}
