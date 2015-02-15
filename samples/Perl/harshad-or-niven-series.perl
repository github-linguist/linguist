#!/usr/bin/perl
use strict ;
use warnings ;
use List::Util qw ( sum ) ;

sub createHarshads {
   my @harshads ;
   my $number = 1 ;
   do {
      if ( $number % sum ( split ( // , $number ) ) == 0 ) {
	 push @harshads , $number ;
      }
      $number++ ;
   } until (  $harshads[ -1 ] > 1000 ) ;
   return @harshads ;
}
my @harshadnumbers = createHarshads ;
for my $i ( 0..19 ) {
   print "$harshadnumbers[ $i ]\n" ;
}
print "The first Harshad number greater than 1000 is $harshadnumbers[ -1 ]!\n" ;
