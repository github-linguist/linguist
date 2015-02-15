sub identity_matrix {
    my $n = shift;
    map {
      my $i = $_;
      [ map { ($_ == $i) - 0 } 1 .. $n ]
    } 1 .. $n;
}

@ARGV = (4, 5, 6) unless @ARGV;

for (@ARGV) {
  my @id = identity_matrix $_;
  print "$_:\n";
  for (my $i=0; $i<@id; ++$i) {
    print join ' ', @{$id[$i]}, "\n";
  }
  print "\n";
}
