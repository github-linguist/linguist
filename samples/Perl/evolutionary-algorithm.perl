use List::Util 'reduce';
use List::MoreUtils 'false';

### Generally useful declarations

sub randElm
 {$_[int rand @_]}

sub minBy (&@)
 {my $f = shift;
  reduce {$f->($b) < $f->($a) ? $b : $a} @_;}

sub zip
 {@_ or return ();
  for (my ($n, @a) = 0 ;; ++$n)
    {my @row;
     foreach (@_)
        {$n < @$_ or return @a;
         push @row, $_->[$n];}
     push @a, \@row;}}

### Task-specific declarations

my $C = 100;
my $mutation_rate = .05;
my @target = split '', 'METHINKS IT IS LIKE A WEASEL';
my @valid_chars = (' ', 'A' .. 'Z');

sub fitness
 {false {$_->[0] eq $_->[1]} zip shift, \@target;}

sub mutate
 {my $rate = shift;
  return [map {rand() < $rate ? randElm @valid_chars : $_} @{shift()}];}

### Main loop

my $parent = [map {randElm @valid_chars} @target];

while (fitness $parent)
   {$parent =
       minBy \&fitness,
       map {mutate $mutation_rate, $parent}
       1 .. $C;
    print @$parent, "\n";}
