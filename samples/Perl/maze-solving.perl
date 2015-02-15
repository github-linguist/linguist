#!perl
use strict;
use warnings;

my ($width, $height) = @ARGV;
$_ ||= 10 for $width, $height;

my %visited;

my $h_barrier = "+" . ("--+" x $width) . "\n";
my $v_barrier = "|" . ("  |" x $width) . "\n";
my @output = ($h_barrier, $v_barrier) x $height;
push @output, $h_barrier;
my @dx = qw(-1 1 0 0);
my @dy = qw(0 0 -1 1);

sub visit {
   my ($x, $y) = @_;
   $visited{$x, $y} = 1;
   my $rand = int rand 4;
   for my $n ( $rand .. 3, 0 .. $rand-1 ) {
      my ($xx, $yy) = ($x + $dx[$n], $y + $dy[$n]);
      next if $visited{ $xx, $yy };
      next if $xx < 0 or $xx >= $width;
      next if $yy < 0 or $yy >= $height;

      my $row = $y * 2 + 1 + $dy[$n];
      my $col = $x * 3 + 1 + $dx[$n];
      substr( $output[$row], $col, 2, '  ' );

      no warnings 'recursion';
      visit( $xx, $yy );
   }
}

visit( int rand $width, int rand $height );

print "Here is the maze:\n";
print @output;

%visited = ();

my @d = ('>>', '<<', 'vv', '^^');
sub solve {
   my ($x, $y) = @_;
   return 1 if $x == 0 and $y == 0;
   $visited{ $x, $y } = 1;
   my $rand = int rand 4;
   for my $n ( $rand .. 3, 0 .. $rand-1 ) {
      my ($xx, $yy) = ($x + $dx[$n], $y + $dy[$n]);
      next if $visited{ $xx, $yy };
      next if $xx < 0 or $xx >= $width;
      next if $yy < 0 or $yy >= $height;

      my $row = $y * 2 + 1 + $dy[$n];
      my $col = $x * 3 + 1 + $dx[$n];

      my $b = substr( $output[$row], $col, 2 );
      next if "  " ne $b;

      no warnings 'recursion';
      next if not solve( $xx, $yy );

      substr( $output[$row], $col, 2, $d[$n] );
      substr( $output[$row-$dy[$n]], $col-$dx[$n], 2, $d[$n] );
      return 1;
   }
   0;
}

if( solve( $width-1, $height-1 ) ) {
   print "Here is the solution:\n";
   substr( $output[1], 1, 2, '**' );
   print @output;
} else {
   print "Could not solve!\n";
}
