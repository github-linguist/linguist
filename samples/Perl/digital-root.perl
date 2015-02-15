#!perl
use strict;
use warnings;
use List::Util qw(sum);

my @digit = (0..9, 'a'..'z');
my %digit = map { +$digit[$_], $_ } 0 .. $#digit;

sub base {
   my ($n, $b) = @_;
   $b ||= 10;
   die if $b > @digit;
   my $result = '';
   while( $n ) {
      $result .= $digit[ $n % $b ];
      $n = int( $n / $b );
   }
   reverse($result) || '0';
}

sub digi_root {
   my ($n, $b) = @_;
   my $inbase = base($n, $b);
   my $additive_persistance = 0;
   while( length($inbase) > 1 ) {
      ++$additive_persistance;
      $n = sum @digit{split //, $inbase};
      $inbase = base($n, $b);
   }
   $additive_persistance, $n;
}

MAIN: {
   my @numbers = (5, 627615, 39390, 588225, 393900588225);
   my @bases = (2, 3, 8, 10, 16, 36);
   my $fmt = "%25s(%2s): persistance = %s, root = %2s\n";

   if( eval { require Math::BigInt; 1 } ) {
      push @numbers, Math::BigInt->new("5814271898167303040368".
      "1039458302204471300738980834668522257090844071443085937");
   }

   for my $base (@bases) {
      for my $num (@numbers) {
         my $inbase = base($num, $base);
         $inbase = 'BIG' if length($inbase) > 25;
         printf $fmt, $inbase, $base, digi_root($num, $base);
      }
      print "\n";
   }
}
