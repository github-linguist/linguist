use List::Util qw(sum);
use POSIX qw(strtol);

sub zip(&\@\@) {
  my $f = shift;
  my @a = @{shift()};
  my @b = @{shift()};
  my @result;
  push(@result, $f->(shift @a, shift @b)) while @a && @b;
  return @result;
}

my @weights = (1, 3, 1, 7, 3, 9);
sub sedol($) {
  my $s = shift;
  $s =~ /[AEIOU]/ and die "No vowels";
  my @vs = map {(strtol $_, 36)[0]} split //, $s;
  my $checksum = sum (zip {$_[0] * $_[1]} @vs, @weights);
  my $check_digit = (10 - $checksum % 10) % 10;
  return $s . $check_digit;
}

while (<>) {
    chomp;
    print sedol($_), "\n";
}
