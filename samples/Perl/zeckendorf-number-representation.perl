my @fib;

sub fib {
  my $n = shift;
  return 1 if $n < 2;
  return $fib[$n] //= fib($n-1)+fib($n-2);
}

sub zeckendorf {
  my $n = shift;
  return "0" unless $n;
  my $i = 1;
  $i++ while fib($i) <= $n;
  my $z = '';
  while( --$i ) {
    $z .= "0", next if fib( $i ) > $n;
    $z .= "1";
    $n -= fib( $i );
  }
  return $z;
}

printf "%4d: %8s\n", $_, zeckendorf($_) for 0..20;
