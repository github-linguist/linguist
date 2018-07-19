sub pi {
  my $nthrows = shift;
  my $inside = 0;
  foreach (1 .. $nthrows) {
    my $x = rand * 2 - 1,
       $y = rand * 2 - 1;
    if (sqrt($x*$x + $y*$y) < 1) {
      $inside++;
    }
  }
  return 4 * $inside / $nthrows;
}

printf "%9d: %07f\n", $_, pi($_) foreach 10**4, 10**6;
