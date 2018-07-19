sub next_swop {
  my( $max, $level, $p, $d ) = @_;
  my $swopped = 0;
  for( 2..@$p ){ # find possibilities
    my @now = @$p;
    if( $_ == $now[$_-1] ) {
      splice @now, 0, 0, reverse splice @now, 0, $_;
      $swopped = 1;
      next_swop( $max, $level+1, \@now, [ @$d ] );
    }
  }
  for( 1..@$d ) { # create possibilities
    my @now = @$p;
    my $next = shift @$d;
    if( not $now[$next-1] ) {
      $now[$next-1] = $next;
      splice @now, 0, 0, reverse splice @now, 0, $next;
      $swopped = 1;
      next_swop( $max, $level+1, \@now, [ @$d ] );
    }
    push @$d, $next;
  }
  $$max = $level if !$swopped and $level > $$max;
}

sub topswops {
  my $n = shift;
  my @d = 2..$n;
  my @p = ( 1, (0) x ($n-1) );
  my $max = 0;
  next_swop( \$max, 0, \@p, \@d );
  return $max;
}

printf "Maximum swops for %2d cards: %2d\n", $_, topswops $_ for 1..10;
