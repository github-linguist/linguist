sub cholesky {
  my $matrix = shift;
  my $chol = [ map { [(0) x @$matrix ] } @$matrix ];
  for my $row (0..@$matrix-1) {
    for my $col (0..$row) {
      my $x = $$matrix[$row][$col];
      $x -= $$chol[$row][$_]*$$chol[$col][$_] for 0..$col;
      $$chol[$row][$col] = $row == $col ? sqrt $x : $x/$$chol[$col][$col];
    }
  }
  return $chol;
}

my $example1 = [ [ 25, 15, -5 ],
		 [ 15, 18,  0 ],
		 [ -5,  0, 11 ] ];
print "Example 1:\n";
print +(map { sprintf "%7.4f\t", $_ } @$_), "\n" for @{ cholesky $example1 };

my $example2 = [ [ 18, 22,  54,  42],
		 [ 22, 70,  86,  62],
		 [ 54, 86, 174, 134],
		 [ 42, 62, 134, 106] ];
print "\nExample 2:\n";
print +(map { sprintf "%7.4f\t", $_ } @$_), "\n" for @{ cholesky $example2 };
