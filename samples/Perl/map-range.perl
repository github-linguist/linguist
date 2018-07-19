#!/usr/bin/perl -w
use strict ;

sub mapValue {
   my ( $range1 , $range2 , $number ) = @_ ;
   return ( $range2->[ 0 ] +
      (( $number - $range1->[ 0 ] ) * ( $range2->[ 1 ] - $range2->[ 0 ] ) ) / ( $range1->[ -1 ]
      - $range1->[ 0 ] ) ) ;
}
my @numbers = 0..10 ;
my @interval = ( -1 , 0 ) ;
print "The mapped value for $_ is " . mapValue( \@numbers , \@interval , $_ ) . " !\n" foreach @numbers ;
