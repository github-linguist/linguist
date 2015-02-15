use List::Util qw(shuffle);

sub bogosort
 {my @l = @_;
  @l = shuffle(@l) until in_order(@l);
  return @l;}

sub in_order
 {my $last = shift;
  foreach (@_)
     {$_ >= $last or return 0;
      $last = $_;}
  return 1;}
