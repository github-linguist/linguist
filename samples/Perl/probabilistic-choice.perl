use List::Util qw(first sum);
use constant TRIALS => 1e6;

sub prob_choice_picker {
  my %options = @_;
  my ($n, @a) = 0;
  while (my ($k,$v) = each %options) {
      $n += $v;
      push @a, [$n, $k];
  }
  return sub {
      my $r = rand;
      ( first {$r <= $_->[0]} @a )->[1];
  };
}

my %ps =
  (aleph  => 1/5,
   beth   => 1/6,
   gimel  => 1/7,
   daleth => 1/8,
   he     => 1/9,
   waw    => 1/10,
   zayin  => 1/11);
$ps{heth} = 1 - sum values %ps;

my $picker = prob_choice_picker %ps;
my %results;
for (my $n = 0 ; $n < TRIALS ; ++$n) {
    ++$results{$picker->()};
}

print "Event   Occurred  Expected  Difference\n";
foreach (sort {$results{$b} <=> $results{$a}} keys %results) {
    printf "%-6s  %f  %f  %f\n",
        $_, $results{$_}/TRIALS, $ps{$_},
        abs($results{$_}/TRIALS - $ps{$_});
}
