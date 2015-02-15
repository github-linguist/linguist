#!/usr/bin/perl
use strict ;
use warnings ;

#whatever the number base, a number stands for itself, and the letters start
#at number 10 !

sub sumdigits {
   my $number = shift ;
   my $hashref = shift ;
   my $sum = 0 ;
   map { if ( /\d/ ) { $sum += $_ } else { $sum += ${$hashref}{ $_ } } }
      split( // , $number ) ;
   return $sum ;
}

my %lettervals ;
my $base = 10 ;
for my $letter ( 'a'..'z' ) {
   $lettervals{ $letter } = $base++ ;
}
map { print "$_ sums to " . sumdigits( $_ , \%lettervals) . " !\n" }
   ( 1 , 1234 , 'fe' , 'f0e' ) ;
