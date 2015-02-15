sub mmult
 {
  our @a; local *a = shift;
  our @b; local *b = shift;
  my @p = [];
  my $rows = @a;
  my $cols = @{ $b[0] };
  my $n = @b - 1;
  for (my $r = 0 ; $r < $rows ; ++$r)
     {
      for (my $c = 0 ; $c < $cols ; ++$c)
         {
          $p[$r][$c] += $a[$r][$_] * $b[$_][$c]
           foreach 0 .. $n;
         }
     }
  return [@p];
 }
